# The Deep Realm — design

**Status:** spec, awaiting G3 review.
**Date:** 2026-08-05
**Campaign:** C2a of the peoples program
(`2026-08-03-the-peoples-program-design.md`). **Runs after C2-0 (The
Generalist) and C2t (The Tolerance), both merged; blocks C2c (The Delvers).**

A cave is the oldest architecture. Before anyone built anything, people lived
in holes that were already there — and the holes that were already there are
still the only part of the world Hornvale models entirely from the outside.
This campaign gives the world an inside.

## 1. What exists, and what does not

The seam was cut deliberately, over three prior campaigns, and most of it is
sitting unused.

**Already built:**

```
  Realm { medium, access }        domains/climate/src/facets.rs
  Stratum                         "the pelagic zones and (later) the
                                   underworld's geological layers are the
                                   same construct at different realms"
  BandKind                        Regolith / Cover / Basement / Roots
                                  (terrain's rock column, since The Lode)
  Deposit.depth: BandKind         ores already sorted into that column
  cave_at(cell) -> Option<Cave>   located caves, typed by the lithologic
                                  process that opened the void
  Cave.depth_reach_bands: 1..=4   how far a void penetrates the column
  dive / surface                  a live vessel verb, with the possession
                                  carrying the stratum it reached
  CauseOfEnd, Function::Extractive, IS_RUIN + a date
```

**Not built:** anything that reads the cave half. A grep of `windows/` and
`cli/` for `Cave` and `CaveKind` returns **no consumers**. Caves are shipped,
located, typed and depth-reaching, and nothing has ever looked at one. That is
rung 2 of this program's own probe-validity ladder — expressible but unread —
sitting in the tree today.

So C2a is mostly *connection*. The exception is §3, which is genuinely new.

## 2. What the underworld is not

Two framings were tried and discarded. Both are recorded because each looked
obviously right until it was checked against the code.

**Not a uniform band column.** The Waterworld's shape — every cell in the realm
holding the same five strata — does not fit rock. Voids are sparse, vary in
number, are absent under most cells, and connect sideways. Bands at a
coordinate cannot express three chambers at odd depths under one cell, none
under its neighbour, and a tunnel between them.

**Not a 3D void field.** A field answers *"is point (x, y, z) rock?"* and
nothing in the codebase ever asks that. The vessel asks *"from here, where can
I go?"*; placement asks *"what lives in this chamber?"* Both are node
questions. The volume was an intermediate representation with no consumer, and
it would have cost a `value_noise_3d` primitive in `kernel/src/noise.rs` — an
explicit permanent save-format contract — plus a `z` on the `Serialize`d
`Position`. Both costs vanish with the volume.

## 3. The substrate: a graph, addressed and derived

**A chamber is a node; a passage is an edge; a chamber's content is a pure
function of its address.**

```
  entrance    terrain's cave_at(cell)          where a descent is possible
  budget      cave.depth_reach_bands 1..=4     how deep this system runs
  chamber     addressed node, content = f(addr)
  passage     edge between chambers            tunnels are the primitive
  depth       BandKind on each node            what the rock here is like
```

Nothing is stored. This is the pattern `RoomAddr { face, path }.seed(Seed(42))`
already uses for surface rooms — but in a **sibling address space**, so
`RoomAddr` itself is untouched.

`Cave.depth_reach_bands` becomes the generator's **budget**: a karst system
with reach 4 grows a graph down to `Roots`; reach 1 is a shallow pocket. How
deep the underworld goes at a place is therefore an existing derived terrain
fact that C2a *spends* rather than authors.

**`BandKind` demotes from navigation to description.** In water a stratum is
something you move *between*; in rock you move between *chambers*, and the band
says how deep you are and what surrounds you. Water is a continuum you swim
through; rock is a solid you move through the gaps in.

### 3.1 An address names a place, never a construction step

An address is a **permanent key** the moment §3.3's override seam has a writer,
because a dug chamber's fact is keyed by it forever. So the format is settled
now, with the care a stream label gets, even though nothing writes one yet.

```
  addresses a CONSTRUCTION STEP        addresses a PLACE          <- REQUIRED
  ---------------------------          --------------------------
  (cell, ordinal)                      (cell, entrance, depth, …)
  (cell, path-from-entrance)           the chamber at this depth here
  the n-th chamber generated
```

If an address names *the fourth chamber the generator made*, any change to
branching, ordering or budget silently relocates every override — the fact
lands on a different room or on none. If it names *the chamber at this depth
under this entrance*, it survives any generator change that does not move the
place itself.

This is the third time the project has met this wall: The Salt ruled an
`EntityId` may be stored, compared and looked up but never *read for its
value*; 0102 found one-per-cell was an index artifact; The Tolerance keyed its
draw on `(site, founded_year)` rather than a mint counter. **Generation order
is never an identity.**

### 3.2 Edge symmetry, and why it is more than a technicality

If chamber A's neighbours derive from A's address and B's from B's, then A→B
and B→A must agree without either consulting a stored graph. **Every edge
derives from the unordered pair of its endpoints**, so both sides hash the same
input. Stated invariant, with a test that walks every passage in both
directions — get it wrong and passages become one-way at random,
deterministically and invisibly until someone walks back.

**The same fact is the reason the underworld is frightening.** A passage is
symmetric: if you can go down, things can come up. Every framing in this spec
says "descend", but the structure is bidirectional, and C2c and C2d both build
on that — a dwarven gate is *closable* precisely because the door works both
ways.

### 3.3 Found or made, and the override seam

A chamber is either **found** or **made**. One taxonomy covers cave-mouth
shelters, Petra, sewers, catacombs, escape tunnels, dwarven halls, drow cities,
a dug shelter, and a hole cut by magic. What separates them is a maker and a
purpose, not a different generator.

This campaign ships **the seam and no digging**:

```
  a chamber's content = its own latest override fact,
                        else its address-derived default
```

That is `hornvale_species::instance_biosphere`'s pattern — *an instance's
effective trait is its own latest override fact, else its kind's authored
default* — the workspace's only instance lens, one level over. The world stores
differences, never itself.

### 3.4 The aperture is a scale, not a boolean

`Access::Delve` as a single value would flatten seven distinct things:

```
  0  sealed        the void exists and is unreachable
  1  a crack       things seep — water, air, small creatures
  2  a cave mouth  shelter, occupied from outside
  3  a worked way  Petra, a cut entrance, a stair
  4  a gate        defended, machinery, CLOSABLE
  5  a shaft net   mines; many apertures, one holding
  6  merged        a settlement half underground
```

**Rungs 0 and 4 earn their place in C2a.** Rung 0 because a void nobody can
reach must still exist — it is what a later dig *finds*, and without it digging
creates rooms out of nothing. Rung 4 because a closable gate is the difference
between a shelter and a fortress, and it is the first thing C2c needs. The rest
is anticipation, and the scale exists so those campaigns extend a rung rather
than widen an enum.

## 4. Depth: whose reach covers it

Something must decide what a place at depth D *is*, so that dwarven halls never
generate beneath a drow city and descent has character rather than uniform
noise. A fixed depth→zone table would prevent the collision and make every
world's descent the same descent.

**Instead, reaches grow from both ends of the column and a régime at depth D is
whoever's reach covers D.**

```
  dwarves dig DOWN from the surface   reach set by holding size, age, history
  the deep lives UP from below        reach set by its own extent
  where the two meet                  the CONTACT ZONE — the frontier
```

Nothing arbitrates the boundary. **Dwarves cannot generate below their own
reach and the deep cannot generate above its own**, so the collision is
structurally impossible rather than checked for — the same trick a process
address space uses when the heap grows up and the stack grows down. The contact
zone falls out as the interesting place instead of being authored.

Because reach is a macro fact, depth character carries *this world's* history:
a large old holding reaches deep, a failed one barely scratches the cover.

**Descent therefore has a forced path:**

```
  surface -> entrance -> natural void -> worked ground -> contact -> deep
```

You cannot reach a deep claim without crossing whoever lies between. That is
not enforced; it simply cannot be expressed, because a claim covers a
contiguous depth range.

## 5. What the macro world decides

**The macro world decides where, whose, how big, and what happened. The derived
interior decides what it looks like.** The provider-tier rule — *coarse
constrains fine; higher fidelity refines, never contradicts* — applied
vertically.

```
  macro (not free)                    derived (free)
  ---------------------------------   -------------------------------
  where an entrance is                floor layouts
  whether anyone lives there          districts, rooms, corridors
  how large the holding is            what is in a given chamber
  whether it still stands, and why    features, dressing, contents
```

**Alignment is anchored at the gate and drifts free with depth.** Deep chambers
owe the surface nothing. The *first* chamber sits behind a specific opening at
a specific altitude and must agree with it, or a gate at 3000 m opens into a
hall that thinks it is at sea level.

### 5.1 Abandonment is history, not a dungeon flag

An `Extractive` community that ended `Fled` in year 1240 **is** Moria, and the
bake emits exactly that today with `IS_RUIN` and a date. Derived as a dungeon
flag, every world's Moria is abandoned for the same non-reason. Read from the
bake, *why* it fell differs per world and can be asked.

C2a does not build this — no people settles underground until C2c. It is
specified so C2c inherits the seam rather than inventing a parallel one.

### 5.2 The underworld is the world's memory organ

The surface erases: weather, growth, decay, and `The Vestige`'s residue model.
Rock retains. Worked stone outlasts every occupant, which is why the underworld
accumulates layers of previous tenants — and why *stratigraphy of occupation*
and *reach from both ends* are two routes to the same structure.

This is also the fast/slow split The Tolerance used for belief and behaviour,
one layer further down: the surface is weather, belief is the ocean, and the
underworld is the rock the ocean sits in.

## 6. Scope

**In:**

1. `Medium::Rock`, a graduated `Access` (§3.4, rungs 0 and 4 live),
   `Realm::UNDERDARK`.
2. The chamber address space (§3.1), the derived-content function, and the edge
   symmetry invariant (§3.2).
3. The override seam (§3.3) — the lookup, not any writer.
4. Depth régime by reach (§4).
5. Descent at the vessel seam, following `dive`/`surface`'s shape.
6. Three `Formation` variants from `CaveKind`: `KarstCave`, `LavaTube`,
   `FractureCave`.
7. Rehoming **xorn** and **rust monster** off their faked surface niches.

**Out, and named so it is not smuggled in:**

- Non-photosynthate supply. Cave ecology here is **allochthonous** — a chamber
  is fed by the cell above, which is the correct first model. Chemosynthetic
  cave life is The Keeping's step D.
- Digging, carving, and any override *writer*.
- Dwarven halls (C2c); drow, duergar, svirfneblin (C2d).
- **The gated-and-fast-changing quadrant** — hazards, sieges, breaches,
  collapse, flood. Named in §7.3 as a place, not built.
  (`MAP-cave-shelter-gamble` is its first instance.)
- The underworld as a **shortcut network** (`MAP-underworld-shortcut`).
- **Undersea caves** (`MAP-undersea-void`).
- 3D noise, and any change to `Position`.
- 0103's unfinished suitability/capacity typing; C2a must not perpetuate raw
  `CellMap<f64>` where it touches that boundary, but does not own the
  conversion.

## 7. Preregistration

Frozen before implementation.

**Task 0 — measure the substrate before designing on it.** Caves have never had
a consumer, so their distribution has never been validated. Over the probe
seeds, before any other work: what fraction of land cells have a cave; the
distribution of `depth_reach_bands`; whether deep access clusters or scatters;
how many cells have reach 4.

*Interpretation, fixed in advance:* if caves are abundant and reach varies, the
substrate is live and §3 proceeds. **If caves are vanishingly rare or almost
none reach past `Regolith`, the underworld is a scattering of shallow pockets**
— report and stop. A realm nobody can get into is not worth a substrate. This
is a genuine gate; The Keeping's Task 0 stopped its own campaign on exactly this
shape of finding.

**H1 — the xorn's fake is measurably gone.** Its niche today approximates
cave-dark with an insolation optimum of 0.05 and near-zero devotion on every
surface axis. After rehoming, it is scored against subterranean conditions and
its surface suitability collapses.

**H2 — the underworld is sparse and irregular, not a second surface.** Chambers
per cell is heavily zero-weighted, and counts vary where non-zero. If every cell
with a cave gets a similar graph, the generator is producing a uniform column
with extra steps — §2's discarded framing returning as an implementation
accident.

**H3 — the mutation.** Setting a cave's `depth_reach_bands` to 1 must collapse
its graph to a shallow pocket. If it does not, the budget is not read and the
terrain coupling is decorative. *(The program's shared acceptance criterion: a
green test proves the code ran; only the mutation proves the axis is visible.)*

**H4 — passages are two-way.** Every edge is traversable in both directions for
every chamber in the probe worlds. This is the §3.2 invariant as a measurement,
not a code review.

**The falsification.** If the underworld can be walked but nothing about it
differs by place — same depth, same shape, same contents everywhere — it is a
dungeon bolted to a worldmap, and the campaign should say so rather than ship
the appearance of integration.

## 8. Flagged for review

1. **This supersedes The Stratum's D3, for the rock realm only.** D3 ruled that
   strata become inhabitable via a *band*, not an address, and discarded a
   `RoomAddr` depth index because it touches a save-format-class type. That
   reasoning is sound for water — every ocean cell holds the same five pelagic
   zones — and it assumed the underground was band-shaped too. It is not. Water
   keeps bands; rock becomes a graph. **D3's actual objection is not incurred:**
   C2a uses a sibling address space and leaves `RoomAddr` untouched. Owes a
   decision record.
2. **The address format is save-format-adjacent** (§3.1). Nothing serializes one
   in C2a, but the moment §3.3 has a writer it is a durable key. Settled now,
   deliberately, with the care a stream label gets.
3. **Task 0 can stop this campaign**, and is meant to be able to.
4. **The escalation is deliberate.** The program spec's C2a paragraph describes
   the sim half only; the owner chose the walk-into-a-cave criterion knowingly
   (2026-08-05): "a roguelike without an underground is incomplete."
5. **Two prior framings were discarded** (§2) after being checked against the
   code. Both looked right. Recorded so a successor does not re-propose them.
6. **Four ideonomy passes** shaped this spec; two of them overturned a framing I
   had already written down. The graph substrate, found-vs-made, the override
   seam, place-addressing and reach-from-both-ends survived the last two passes
   unchanged, which is mild evidence they are load-bearing rather than merely
   mine.
