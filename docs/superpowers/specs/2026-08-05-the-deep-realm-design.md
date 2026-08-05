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
  dive / surface                  a live vessel verb: descend a layer of the
                                  water column, with the possession carrying
                                  the stratum it reached
```

**Not built:** anything that reads them. A grep of `windows/` and `cli/` for
`Cave` and `CaveKind` returns **no consumers**. Caves are shipped, located,
typed and depth-reaching, and nothing has ever looked at one. That is rung 2 of
this program's own probe-validity ladder — expressible but unread — sitting in
the tree today.

So C2a is mostly *connection*, not construction. The exception is the substrate
in §3, which is genuinely new.

## 2. What the underworld is not

Two framings were tried and discarded during the brainstorm. Both are recorded
because each looked obviously right until it was checked.

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
says how deep you are and what surrounds you. The divergence is honest: water
is a continuum you swim through, rock is a solid you move through the gaps in.

### 3.1 Edge symmetry is the one hard problem

If chamber A's neighbours derive from A's address and B's from B's, then A→B
and B→A must agree without either consulting a stored graph. Get this wrong and
passages become one-way at random — deterministically, reproducibly, and
invisibly until someone walks back.

**Every edge must derive from the unordered pair of its endpoints**, so both
sides hash the same input. This is a stated invariant with a test that walks
every passage in both directions, not a convention.

### 3.2 Found or made — and the override seam

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
differences, never itself, which is the same bargain it already makes with its
seed.

Anticipating the seam costs an override lookup and this paragraph. Retrofitting
it would make every "just derived" chamber a special case the day anything
carves, digs, collapses or is cut open.

## 4. What the macro world decides

**The macro world decides where, whose, how big, and what happened. The derived
interior decides what it looks like.** This is the provider-tier rule — *coarse
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
owe the surface nothing. The *first* chamber sits behind a specific opening at a
specific altitude and must agree with it; if it does not, a gate at 3000 m
opens into a hall that thinks it is at sea level.

**Depth needs a régime.** With unbounded derived depth, nothing may place
dwarven halls beneath a drow city by accident. A depth→character function is
cheap and must be stated rather than left emergent.

### 4.1 Abandonment is history, not a dungeon flag

The history bake already emits what a fallen holding needs:

```
  CauseOfEnd { Famine, Burned, Plague, Fled, Migrated }
  Function   { Agrarian, Extractive (ore, stone, salt), ... }
  IS_RUIN + a date
```

An `Extractive` community that ended `Fled` in year 1240 **is** Moria. Derived
as a dungeon flag, every world's Moria is abandoned for the same non-reason.
Read from the bake, *why* it fell differs per world, and can be asked.

C2a does not build this — no people settles underground until C2c. It is
specified here so C2c inherits the seam rather than inventing a parallel one.

## 5. Scope

**In:**

1. `Medium::Rock`, `Access::Delve`, `Realm::UNDERDARK`.
2. The chamber address space, the derived-content function, and the edge
   symmetry invariant (§3.1).
3. The override seam (§3.2) — the lookup, not any writer.
4. Descent at the vessel seam: a verb that enters a cave mouth and moves
   between chambers, following `dive`/`surface`'s shape.
5. Three `Formation` variants from `CaveKind`: `KarstCave`, `LavaTube`,
   `FractureCave`.
6. Rehoming **xorn** and **rust monster** off their faked surface niches onto
   subterranean conditions.

**Out, and named so it is not smuggled in:**

- Non-photosynthate supply. Cave ecology here is **allochthonous** — a chamber
  is fed by the cell above it, which is the correct first model. Chemosynthetic
  cave life is The Keeping's step D.
- Digging, carving, and any override *writer*.
- Dwarven halls (C2c), drow/duergar/svirfneblin (C2d).
- Cave hazards — collapse, flood, bad air (`MAP-cave-shelter-gamble`).
- 3D noise, and any change to `Position`.
- Suitability/capacity typing (0103's unfinished step); C2a must not perpetuate
  raw `CellMap<f64>` where it touches that boundary, but it does not own the
  conversion.

## 6. Preregistration

Frozen before implementation.

**Task 0 — measure the substrate before designing on it.** Caves have never
had a consumer, so their distribution has never been validated. Over the probe
seeds, before any other work: what fraction of land cells have a cave at all;
the distribution of `depth_reach_bands`; whether deep access clusters or
scatters; and how many cells have an entrance whose reach is 4.

*Interpretation, fixed in advance:* if caves are abundant and reach varies, the
substrate is live and §3 proceeds. **If caves are vanishingly rare or almost
none reach past `Regolith`, the underworld is a scattering of shallow pockets**
— and the campaign reports that and stops, because a realm nobody can get into
is not worth a substrate. This is a genuine gate; The Keeping's Task 0 stopped
its own campaign on exactly this shape of finding.

**H1 — the xorn's fake is measurably gone.** Its condition niche today
approximates cave-dark with an insolation optimum of 0.05 and near-zero
devotion on every surface axis. After rehoming, its suitability is scored
against subterranean conditions, and its surface suitability collapses.

**H2 — the underworld is sparse and irregular, not a second surface.** The
distribution of chambers per cell is heavily zero-weighted, and chamber counts
vary where they are non-zero. If every cell with a cave gets a similar graph,
the generator is producing a uniform column with extra steps — which is the
framing §2 discarded, reappearing as an implementation accident.

**H3 — the mutation.** Setting a cave's `depth_reach_bands` to 1 must collapse
its chamber graph to a shallow pocket. If it does not, the budget is not being
read and the terrain coupling is decorative. *(The program's shared acceptance
criterion: a green test proves the code ran; only the mutation proves the axis
is visible.)*

**The falsification.** If the underworld can be walked but nothing about it
differs by place — same depth, same shape, same contents everywhere — then it
is a dungeon bolted to a worldmap, and the campaign should say so rather than
ship the appearance of integration.

## 7. Flagged for review

1. **This supersedes The Stratum's D3, for the rock realm only.** D3 ruled that
   strata become inhabitable via a *band*, not an address, and discarded a
   `RoomAddr` depth index because it touches a save-format-class type. That
   reasoning is sound for water — every ocean cell holds the same five pelagic
   zones — and it assumed the underground was band-shaped too. It is not.
   Water keeps bands; rock becomes a graph. **D3's actual objection is not
   incurred:** C2a uses a sibling address space and leaves `RoomAddr`
   untouched. Owes a decision record.
2. **A new address space is save-format-adjacent.** Chamber addresses are not
   serialized in C2a (content derives, nothing is stored), but the moment §3.2's
   override seam has a *writer*, an address becomes a durable key. The address
   format should therefore be settled with the same care as a stream label,
   even though nothing writes one yet.
3. **Task 0 can stop this campaign**, and is meant to be able to.
4. **The escalation is deliberate.** The program spec's C2a paragraph describes
   the sim half only; the owner chose the walk-into-a-cave criterion knowingly
   (2026-08-05): "a roguelike without an underground is incomplete."
5. **Two prior framings were discarded** (§2) after being checked against the
   code. Both looked right. Recorded so a successor does not re-propose them.
