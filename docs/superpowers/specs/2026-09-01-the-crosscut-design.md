# The Crosscut — the underworld gets somewhere to go around

**Campaign:** The Crosscut · **Branch:** `campaign/the-crosscut` ·
**Decision block:** 0566–0575 (main ceiling 0516 at reservation) ·
**Drafted:** 2026-09-01 · **Status:** Draft, at G3.

**The Circuit, campaign 1** ([metaplan](2026-09-01-the-circuit-metaplan.md)).
Predecessors: The Adit (the level generator), The Stope and The Drift (the
branch lattice), The Gallery (walking it). **Campaign ledger:**
`docs/superpowers/ledgers/2026-09-01-the-crosscut.md`.

*A crosscut is a tunnel driven across the vein to join two drifts. It is the
part of a mine that turns two dead ends into a loop, and a mine drives them
because air has to flow: a working with no loop cannot breathe.*

## 1. What occasioned it

Three facts about the shipped underworld, each verified in source on
`origin/main` at `18f63ebfa` (2026-09-01):

1. **A level's region graph is a tree, by construction.**
   `region::build_node` (`windows/vessel/src/underworld_level/region.rs`)
   makes binary splits to depth ≤ 2; `connect_split_boundaries`
   (`underworld_level/mod.rs`) carves exactly one passage between each
   split's two children. `n` leaves, `n − 1` passages, no cycle at the region
   level. Whatever loops a cellular-automaton leaf carves inside itself are
   texture, not structure — nothing can be *said* about them.
2. **A branch is a line, and every level has one way down and one way up.**
   `passages_from` (`windows/worldgen/src/chamber.rs`) offers level `k`
   exactly one successor within a run. `place_connections` cuts one
   `StairsDown` in the first leaf and one `StairsUp` in the last, and
   `stairs_down_and_stairs_up_never_share_a_cell` asserts that count across
   200 seeds. `Underground::peek_stairs` (`windows/vessel/src/underground.rs`)
   documents that it *relies* on "exactly one" to know where a stairway lands.
3. **Neither embedder can realize a cycle.** `lattice/allocate.rs`:
   "`structure_at` builds a PATH graph, so the recursion is a chain."
   `lattice/grow.rs`: "asserts a chain." There is no graph-rewriting code
   anywhere in the workspace (the Explore survey's grep for rewrite /
   production hit only `domains/language`).

So the whole underworld is the structure Dormans' essay opens by rejecting:
"the structure of the generated dungeon essentially is a branching tree: it
will have many dead ends and force the player to track back frequently." It
got there honestly — each campaign built the minimal next thing — and the
minimal next thing now is the one primitive none of them needed.

## 2. Keystone

> **A place is a graph before it is a map, and the graph is series-parallel.**

A branch's plan is generated first, for the whole run of levels, as a graph
grown by exactly two operations — lengthen a path (series) and split an edge
into two paths (parallel) — on a per-level grid of regions. Because the
grammar is series-parallel, the derivation tree is the realm tree, planarity
is free, and a cycle may put one of its two paths on the floor below. The
level generator then *realizes* that plan: each node becomes a region carved
by one of the Adit's existing algorithms, each same-floor edge a passage
through a shared wall, each cross-floor edge a stairway whose two ends share a
coordinate. Nothing about a level is drawn that the plan did not say.

## 3. The design

### 3.1 The plan graph

```
  DescentPlan { rungs: Vec<Band>, nodes: Vec<Node>, edges: Vec<Edge>,
                entrance: NodeId, terminus: NodeId, realms: Vec<Realm>,
                dof: u32 }

  Node  { level: u8, cell: GridCell { col, row },
          depth: u16,            hops from entrance along shortest path
          realm: Option<RealmId> }  innermost parallel composition holding
                                 it; None for a spine node on no cycle
  Edge  { a: NodeId, b: NodeId, kind: Passage | Stair { at: (x, y) } }
          Passage: same level, grid-adjacent cells
          Stair:   adjacent levels, SAME grid cell, one shared coordinate
  Realm { parent: Option<RealmId>, path_a: Vec<NodeId>, path_b: Vec<NodeId>,
          class: LengthClass }   Dormans' four: LongLong | LongShort |
                                 ShortLong | ShortShort, from |a| vs |b|
```

**The unit is the descent a player walks, and this corrects the draft
Nathan approved.** The G3 draft keyed the plan to a `RunAddr` and said it
refined `passages_from`. Reading `Underground::enter`
(`windows/vessel/src/underground.rs`) for the plan showed that the walked
object is different: `enter` builds **one level per habitation rung**
(`hornvale_terrain::rungs()` minus `Surface`, five today) under one
vertex, and reads neither `levels_in_branch`, `passages_from` nor any
branch — the Stope/Drift lattice has no walking consumer, which
`MAP-chamber-occupancy` already records. A plan keyed to a run would have
described an object nobody stands in. So the plan is keyed to
`(seed, vertex)` and covers the five rungs `enter` generates, in that
order; `level ℓ` is `habitation_rungs()[ℓ]`. Wiring the walk to the branch
lattice is a real gap and a separate campaign; this one does not widen
into it (ledger #7).

The grid for level `ℓ` has `cols × rows` cells with `cols = w / REGION_SPAN`
and `rows = h / REGION_SPAN`, where `(w, h)` is the extent
`generate_level_extent(rung)` already gives (its formula moves to
`worldgen` unchanged so the plan can read it) and `REGION_SPAN = 8`
matches the Adit's `MIN_REGION_SPAN`. A level at rank 0 (40×24) yields 5×3;
the deepest rank (56×32) yields 7×4. The constant is named once; the grid is
a *derived* shape of the extent, not an authored count.

**Coarse constrains fine.** The plan never adds a vertical connection the
descent lacks: a `Stair` edge joins level `ℓ` to `ℓ+1` only, never skipping
a rung, and the deepest level's terminus carries the same dangling
`StairsDown` it does today, so `STAIRS_LEAD_NOWHERE_REFUSAL` keeps its one
firing case. The plan *refines* "down one rung" into "which stairways",
never contradicts it.

### 3.2 Growth: two operations, one budget, one stream

From one derived stream `underworld/plan/v1` with four legs (`spine`,
`cycle`, `extend`, `stair`), each named in `windows/worldgen/src/streams.rs`
and published through `stream_labels()`, each further keyed by the vertex
the way `character_of` keys its draw (`StreamLabel::dynamic`):

1. **Spine.** Choose the entrance cell on level 0's west edge and, on each
   level, a drawn stair cell distinct from the arrival cell. Lay a grid path
   from arrival to stair cell (a deterministic breadth-first search over
   free cells, fixed neighbour order N, E, S, W); add a `Stair` edge to the
   same grid cell on the next level, which is that level's arrival. On the
   deepest level the drawn cell is the terminus. The spine is one series
   composition and touches every level once. (`spine` leg.)
2. **Parallel — `cycle(u, v)`.** Pick a `Passage` edge `u–v` on the level
   and walk from `v` away from `u` along degree-two nodes for a drawn 0–2
   further hops to `v'`; `path_a` is that existing segment (length 1–3).
   Find `path_b`: a grid path from `u` to `v'` through cells not yet used,
   node-disjoint from `path_a` except at its endpoints. **Same-floor** when
   the draw says so and the search succeeds on this level; **cross-floor**
   when the draw says so, `ℓ+1` exists, and both `u`'s and `v'`'s grid cells
   are free on `ℓ+1`: then `path_b` descends at `u`, runs along level `ℓ+1`,
   and climbs at `v'` — two stairways, one cycle across two floors. Record a
   `Realm` with both paths and its `LengthClass`; a realm anchored on level
   `ℓ` counts toward `ℓ`'s budget. (`cycle` and `stair` legs.)
3. **Series — `extend(u, v)`.** Replace a `Passage` edge with a grid path
   `u → … → v` of two or more hops through free cells, removing the direct
   edge. Lengthens a path so later cycles have somewhere to attach and so
   length classes vary. (`extend` leg.)
4. **Nesting is not a third operation.** Applying `cycle` to an edge that is
   already inside a realm produces Dormans' nested cycle (Fig. 9.2) and the
   new realm's `parent` is set. The derivation tree that results *is* the
   series-parallel decomposition, and `Node.realm` is read off it.
5. **Budget.** Each level `ℓ` has a target cycle count

   ```
   target(ℓ) = base(CaveKind) + worked(ℓ)          clipped to [1, 5]
     base:   LavaTube 1   Fracture 2   Karst 3
     worked: +1 if ChamberOrigin::Made or Character::DrowTier at ℓ, else 0
   ```

   Growth stops when every level meets its target or no legal `cycle` move
   remains. The `[1, 5]` clip is Dormans' own finding ("two to five cycles
   give each level a distinct and recognizable shape … adding more cycles
   just seems to clutter") and is the one authored constant in the grammar;
   it is named as such. `base` is *derived from the rock*: karst is a maze
   because water dissolves many routes, a lava tube is a single conduit, a
   fracture system is in between. `worked` is derived from *workmanship*: a
   mine must have loops because it must ventilate — an intake and a return
   airway — and a dead-end drift is the unventilated one. Neither number is
   "for fun".
6. **Attributes.** `depth` by breadth-first search from the entrance;
   `realm` from the decomposition tree; `LengthClass` per realm with the
   threshold "long" = strictly more than the shorter path's length plus one
   — stated once, tested, and exported. **This campaign computes and
   exports them and reads none of them.** The Brattice reads `LengthClass`
   to select a pattern; The Plat reads `depth` and `realm` as the intimacy
   gradient and the circulation realm.

`dof` counts every draw as it is made, mirroring `Level.dof` and
`Lattice.dof` — The Blocking's fidelity discipline, applied to the generator
that produces the truth the realizer must not exceed.

### 3.3 Realization: the plan becomes a level

`generate_level_*` in `windows/vessel/src/underworld_level/` takes the
`BranchPlan` and the level index instead of building a partition tree:

- Every grid cell **in the plan** becomes a region: its rectangle, inset by
  one for fabric, is carved by the existing `carve::Algorithm` chosen by the
  existing `choose_leaf_style` logic. Every grid cell **not in the plan** is
  rock. The Adit's leaf carvers, worked/natural inertia and flooding rule
  are untouched; only the scaffold that hands them rectangles changes.
- Every `Passage` edge is realized as today's `connect_cells` between the
  nearest walkable pair across the shared wall, once. Two adjacent regions
  with **no** edge keep a solid wall between them — that is where the
  grammar's non-adjacency becomes the player's "no way through here", and
  it is the property the partition tree could not express (siblings were
  always joined).
- Every `Stair` edge is realized as a `StairsDown` at coordinate `c` on
  level `ℓ` and a `StairsUp` at the **same coordinate `c`** on level `ℓ+1`,
  where `c` is drawn (from the `stair` leg, at plan time, so the vessel
  spends no draw) inside the intersection of the two regions' rectangles.
  That intersection is asserted non-empty for every rung pair the ladder
  admits, as a test, not assumed. A landing cell is set walkable even if
  the carve left it rock: a stairway has a foot.
- **Stairs pair by coordinate.** `peek_stairs` stops relying on "exactly
  one" and lands on the cell with the same coordinates one rung over — the
  same physical stairway named from its other end, which is what its own
  doc already says it is. `stairs_down_and_stairs_up_never_share_a_cell`
  is **replaced, not deleted**: its successor asserts that every
  `StairsDown` on rung `ℓ` has a `StairsUp` at the same coordinate on rung
  `ℓ+1` and vice versa, and that no cell carries both.
- `UNDERWORLD_LEVEL_PARTITION` is **retired outright**, not bumped: nothing
  derives from it after this campaign, and a `/v2` of a leg no world reads
  would mint an empty epoch — the case The Drift's amendment A.6 refused.
  Its absence is asserted from `cli/src/streams.rs`'s stamp roster, as
  `chamber/branch-root/v1`'s is.

### 3.4 What the realm tree buys, stated so it can be checked

Because every operation is series or parallel, three properties hold by
construction and are each pinned by a unit test rather than measured:

- **Planar and grid-embedded**: every edge joins grid-adjacent cells on one
  level or the same cell on adjacent levels, so there is never a crossing to
  resolve and never an embedding to fail.
- **Every node is reachable from the entrance**: growth only ever adds paths
  between nodes already connected. The Drift's 100% is inherited at region
  scale rather than re-won.
- **Every node has exactly one innermost realm**: the decomposition tree is
  a tree. The Plat's "which circulation realm am I in" and The Brattice's
  "which cycle does this lock guard" have one answer each.

What the grammar **cannot** express, deliberately: a shortcut between two
*different* cycles (a K4 minor — no longer series-parallel, and the case
where two realms would both own a room); a junction into another cave
system; a cycle through the next band's branch. Each is a captured
follow-up, not an accident of scope.

## 4. Preregistration

Measured over the standing seed panel (42, 7, 1234), every cave-bearing
vertex of each, by a readout `hornvale circuit --seed <N>` that
`scripts/regenerate-artifacts.sh` writes to the committed audit page
`docs/audits/underworld-circuit-seed-panel.md` — the exact shape
`hornvale underworld` and `docs/audits/underworld-lattice-seed-panel.md`
already have — declared **as a file** in `docs/generated-paths.txt`, per the
already-declared-directory hazard. (The G3 draft also named The Gallery's
60-seed sweep; a committed page must regenerate in seconds, and the
three-seed panel over every cave vertex is 3,821 descents, which is the
larger sample anyway.) Frozen here, before the code.

### 4.1 Loop share

For each region other than the entrance, does there exist a return route to
the entrance edge-disjoint from the route that reached it (Menger: two
edge-disjoint paths, i.e. the region lies in the entrance's 2-edge-connected
component)? **Loop share** = such regions / all non-entrance regions, per
run.

- Today, by construction: **0** on every run (Task 0 measures it anyway;
  see the branch table there).
- **Prediction:** median loop share over the panel ≥ **0.50**. Below 0.50
  is FALSIFIED and is the headline.

### 4.2 Density ordering

Realms anchored per level (each realm adds exactly one to the descent's
Kirchhoff mesh count `E − V + 1`, so the sum over levels is the cyclomatic
number), grouped by `CaveKind` and by workmanship.

- **Prediction:** panel medians strictly ordered
  `LavaTube < Fracture < Karst`, and `DrowTier > WildCave` within each kind
  that has both. (`Made` is in the rule but not in the readout: no reachable
  world carries it until The Plat writes `ChamberOverrides`, so only the
  `Character` half of `worked` can be measured here.) A tie or inversion
  anywhere is FALSIFIED.
- Every level's count lies in `[1, 5]` — a **guard**, asserted, not a
  prediction.

### 4.3 Cross-floor cycles

Share of descents holding at least one realm whose two paths touch
different levels.

- **Prediction:** ≥ **0.25** of descents. This is the multi-floor
  claim of the whole program made falsifiable; a null here means the
  cross-floor move is drawn too rarely to matter and is a finding, not a
  tuning invitation.

### 4.4 Semilattice overlap (report only)

Among regions on any cycle, the share lying in ≥ 2 realms (a node on the
shared path of a nested cycle). Alexander's *A City Is Not a Tree*
criterion, measured for the first time; no threshold, because nothing yet
says what a good number is. The frontier row that named it a "candidate
Lab metric" gets its first column.

### 4.5 Determinism and fidelity

Two independent builds of every plan and level on the panel are
byte-identical; `BranchPlan.dof` equals the number of draws the four legs
made (counted at the draw, compared to a recount by an independent walk of
the decomposition tree), and `Level.dof` no longer includes a partition
draw. Standard, and the campaign does not close without it.

## 5. Save-format and determinism consequences

**None by design, and the design says how it knows.** Every plan and level
is `FRAME`-tier under decision 0069 — derived on entry, discarded on exit,
never serialized. New stream labels are additive (`underworld/plan/v1` and
its four legs); one label is retired (§3.3); no existing stream's
consumption order changes. No committed fact keys on a level cell: the
Latch's `passage-cleared` names a `ChamberAddr` (a cave mouth), the Chattel's
things key on a room facet, and creature position is a chamber.

**The check is executable, and Task 0 runs it before any code**, with a
branch table rather than a prediction:

```
  grep for any committing path that reads Level / LevelCellKind / CellGrid
    none found                      -> proceed; FRAME-tier holds
    a committing reader found       -> STOP. Every level in every world
                                       changes shape under this campaign, so
                                       that reader makes it an EPOCH. Return
                                       to G3 with the reader named.
  byte-golden fixtures that embed a level render
    none                            -> nothing to rebaseline
    some                            -> list them in the plan; REBASELINE=1
                                       with the diff reviewed, as a task
```

`docs/audits/underworld-lattice-seed-panel.md` witnesses the *chamber*
lattice (existence, reach, run lengths) and reads no level interior; Task 0
regenerates it and records the diff rather than asserting it is empty.

## 6. Non-goals

Locks, keys, valves, danger, secrets, hoards (The Brattice) — the plan
exports `LengthClass` and stamps nothing. Residents, the intimacy reading,
the hoarder, `ChamberOverrides` (The Plat). Junctions and cross-run cycles
(`MAP-cross-run-cycles`). Vaults (`MAP-underworld-vaults`). Any change to
`lattice/`, to the chamber address, to `passages_from`, or to the band
transition at a run's bottom. Camera-follow (`MAP-underworld-viewport`).
Prose for what a cycle looks like (`MAP-underworld-dressing`).

## 7. Acceptance

1. A level of seed 42's first cave system contains a region whose plan node
   lies on a cycle, and a scripted `possess` fixture walks from the arrival
   stair to that region by one route and back by the other, never
   re-entering a region — a cycle a player can choose between, walked rather
   than read off the graph.
2. Somewhere on the panel, a stairway down leads to a floor whose route
   returns you to the floor above by a *different* stairway — a cross-floor
   cycle walked end to end through the session's own `down`, direction and
   `up` verbs in a test, not inferred from a graph.
3. `up` and `down` land on the paired stairway's own cell on every stair of
   every level of the panel, and `STAIRS_LEAD_NOWHERE_REFUSAL` still fires
   only on the terminus of a run whose band admits nothing below.
4. The four preregistered readouts (§4.1–4.4) are on the committed audit
   page with their verdicts, PASSED or FALSIFIED, in the words frozen above.
5. The Adit's leaf algorithms, worked/natural inertia and flooding rule
   produce the same *kinds* of interior they did — pinned by the existing
   carve tests passing unmodified.

## 8. Task shape (detail belongs to the plan)

0. **Baseline and premise check.** Measure §4.1–4.2 on today's generator
   (branch table: 0 everywhere → the tree claim stands; anything else →
   STOP, the premise in §1 is wrong and the plan re-reads `region.rs`). Run
   the §5 greps and record results. Regenerate the lattice audit and record
   its diff.
1. **`windows/worldgen/src/circuit.rs`** — types, the four legs, spine,
   `cycle`, `extend`, budget, attributes; unit tests for the three §3.4
   invariants, budget bounds, stream-leg identity, determinism, `dof`.
2. **Realizer** — grid regions replace the partition tree; passages from
   edges; stairs by coordinate; retire `UNDERWORLD_LEVEL_PARTITION`;
   replace the stairs test with the pairing invariant.
3. **`peek_stairs` pairs by coordinate**; session verbs unchanged; a
   scripted fixture walks a cross-floor cycle (acceptance 2).
4. **Probe, audit page, `generated-paths.txt` entry**; readouts written in
   the frozen words.
5. **Book and close** — chronicle, frontier sweep
   (`MAP-underworld-traversal-grammar` → shipped, slice 1;
   `TOOL-underworld-embedder-unification` answered; `CLIENT-semilattice-
   caution` gains its first measurement), retrospective, decisions
   0566–0568 minted, `make sluice-stage` at every stage boundary.

## 9. Decisions this campaign expects to mint

- **0566** — A place is a graph before it is a map, and the grammar is
  series-parallel (the structural primitive is the cycle; two operations;
  the derivation tree is the realm tree).
- **0567** — Stairs pair by coordinate: a stairway's two ends share a cell,
  and that is how a landing is found.
- **0568** — Cycle density is derived from rock and workmanship, never
  authored; the `[1, 5]` clip is the one authored constant and is named as
  Dormans'.

## 10. Provenance

The Circuit metaplan §5. Nathan's request of 2026-09-01 and his brainstorm
of 2026-08-28 (`MAP-underworld-traversal-grammar`). The ideonomy pass that
shaped §2–§3 (three operators, two organons, five dimension prompts) is
recorded in the campaign ledger; its overturns were: the series-parallel
framing (from the electrical-circuit re-instantiation: cycles are meshes,
nesting is a mesh sharing an edge), derived density (from mine ventilation
and karst hydrology), and the swap of main and side effect (a cycle is the
residents' circulation first, the intruder's puzzle second), which became
the program's keystone.
