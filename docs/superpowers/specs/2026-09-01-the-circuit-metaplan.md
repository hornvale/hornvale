# The Circuit — the program for places you can go around in

*Metaplan. Three campaigns; the first is specced beside this file as
[The Crosscut](2026-09-01-the-crosscut-design.md). Status: all unstarted.
Drafted 2026-09-01 under autopilot; Nathan reviews at G3.*

## 1. What occasioned it

Nathan brought Joris Dormans' essay *Cyclic Generation* (Short & Adams,
*Procedural Generation in Game Design*, ch. 9) and asked how it combines with
Christopher Alexander's pattern language to give the underworld multi-floor
places — dragon's lairs, drow cities, delvings — that "operate more like
authored areas than simple disconnected randomized floors."

The registry already holds the question's outline.
`MAP-underworld-traversal-grammar` (elaborated, Nathan's brainstorm of
2026-08-28) names a pattern language for how you *move* through a place, and
the frontier essay behind it argues the Hearth's `requires` relation is the
primitive. Dormans supplies what that essay is missing — the *structural*
half — and the shape of the gap is exact once the shipped code is read:

- **Every scale of the underworld is a tree or a path today.** A branch is
  levels `0..n` in a line (`passages_from`, `windows/worldgen/src/chamber.rs`).
  A level is a binary partition tree whose siblings are joined by exactly one
  passage per split (`connect_split_boundaries`,
  `windows/vessel/src/underworld_level/mod.rs`), so its region graph has
  `n` leaves and `n-1` edges. Every level has exactly one stair down and one
  up, placed in the first and last leaf, and the descent walk's `peek_stairs`
  (`windows/vessel/src/underground.rs`) relies on that count. This is the
  "drilling out" shape Dormans' opening paragraph describes — many dead ends,
  constant backtracking — arrived at by the most natural sequence of
  campaigns.
- **`requires` cannot make a cycle.** It is a dependency edge: a pattern is
  admitted once the pattern it completes is present. It gates; it does not
  rewrite. Producing a cycle needs an operation the Hearth never needed — take
  an edge and replace it with two paths — and that operation is what Dormans'
  graph grammar contributes. The frontier essay's sentence "very little of it
  needs inventing" understated this by exactly one primitive.
- **Both embedders assert a chain.** `lattice/allocate.rs` ("`structure_at`
  builds a PATH graph, so the recursion is a chain") and `lattice/grow.rs`
  ("asserts a chain") cannot realize a cyclic adjacency graph. Dormans' own
  answer — generate the graph *on a grid* so that graph-to-tilemap is trivial
  — is the one route where a cycle costs nothing to embed.

Alexander enters not as decoration but as the second reading of the same
skeleton. Dormans says cycles were "inspired by previous research in
architecture, urban planning" and that "in most cities, buildings, and parks,
you can go around in circles"; that is *A City Is Not a Tree* restated for
level design. Alexander's **98 Circulation Realms**, **131 The Flow Through
Rooms**, **127 Intimacy Gradient**, **129 Common Areas at the Heart** and
**112 Entrance Transition** are all statements about the cycle structure of
inhabited space and the gradient along it. Dormans' lock-and-key cycle and
Alexander's intimacy gradient are one structure read by two people: the
resident walks a circulation loop with privacy deepening along it; the
intruder walks the same loop and meets gates.

## 2. Keystone

> **You can go around in circles.** A place is a graph before it is a map,
> the graph has cycles because places do, and one skeleton carries two
> readings — the residents' circulation and the intruder's puzzle.

Three commitments follow and bind every campaign:

1. **Structure first, at branch scope.** The plan for a whole run of levels is
   generated before any level is carved (Dormans: "a plan for the dungeon is
   generated before even one level"), so a cycle may span floors and a key may
   sit a floor below its lock.
2. **The grammar is series-parallel.** Every structural operation is a series
   composition (lengthen a path) or a parallel composition (split an edge into
   two paths), so the derivation tree *is* the realm tree, the graph is planar
   by construction, and "which cycle owns this room" has one answer.
3. **Nothing is authored but rules, and density is derived.** Cycle patterns
   are a small inventory of relational rules (decision 0011; the Hearth's
   discipline); how many cycles a place has comes from what the place is —
   karst water finds many ways, a lava tube finds one, and a worked mine must
   breathe through loops of intake and return air — never from a tuning
   constant labelled "fun".

## 3. The campaigns

    1. THE CROSSCUT        structure: the series-parallel plan graph per branch
                           run, grown on a per-level region grid, cycles that
                           span floors through paired stairs, regions carved by
                           the Adit's existing leaf algorithms, depth and realm
                           attributes computed and exported. Cycle density
                           derived from CaveKind and workmanship. Preregistered
                           loop-share and density-ordering readouts. No gates.

    2. THE BRATTICE        gates: Dormans' cycle-pattern inventory (Fig. 9.8)
                           as data over the Crosscut's length classes; the
                           lock-and-key attribute taxonomy (conditional /
                           dangerous / uncertain; permanent / collapsing;
                           valve / asymmetric; safe / unsafe) as edge and
                           node attributes; doors as Things (The Chattel),
                           keys placed with Dormans' safety rule; the DROP as
                           the cross-floor valve; capability keys (Swim, Fly)
                           unreserving The Gallery's MovementMode seams.

    3. THE PLAT            the inhabited reading: Alexander's intimacy
                           gradient, common heart and entrance transition as
                           the vocabulary a RESIDENT people draws, keyed to
                           Character / ChamberOrigin::Made; the dominant
                           creature as a hoarder placed by the energy field
                           (frontier §traversal-grammar), never a fight (0070);
                           the first production writer of ChamberOverrides so
                           that a Made chamber exists in a world a player can
                           reach. Kin to The Precincts (surface districts,
                           Rose Window rung 2), which shares the grammar and
                           may share the scaffold.

Each campaign is a hard prerequisite for the next: no gate without a graph to
hang it on, no residents' reading without gates for the intruder's. Campaigns
2 and 3 are named here and specced when their own brainstorms happen.

## 4. What is deliberately NOT in this program

- **Authored vaults** (`MAP-underworld-vaults`). Still the separable,
  heavier decision The Delving named. A cycle pattern is smaller than a room;
  a vault is larger than one.
- **Cross-run cycles.** `junctions_at` joins two cave systems at one band and
  `descents_from` can name several branches below a run's bottom; a cycle
  that goes down branch A, across a junction and up branch B is the NetHack
  Mines loop. Captured as `MAP-cross-run-cycles`; The Crosscut's plan scope
  is one run, and a band transition keeps today's single-stair convention.
- **A general graph-rewrite engine.** Dormans' Ludoscope matches arbitrary
  left-hand sides. With at most a few dozen nodes per level and a
  series-parallel grammar, four operations in code cover every rule in the
  essay; a matcher would be machinery in search of a rule.
- **Extending `lattice/` to rectangular duals.** The buildings embedder stays
  a chain embedder of a fixed anchor graph. The question
  `TOOL-underworld-embedder-unification` asked is answered by this program in
  the negative for the *code* and in the affirmative for the *discipline*: both
  layers are fidelity embedders reporting residual degrees of freedom.
- **Surface settlements and districts.** The Precincts (Rose Window
  metaplan) owns that; The Plat records what it learns for it.
- **Desire paths** (`CLIENT-desire-paths`). The emergent version of a cycle —
  worn by residents rather than drawn — is rung 4 of the furnishing ladder
  and stays there.
- **Camera-follow** (`MAP-underworld-viewport`). A level with fifteen regions
  will walk `you` off the 40-column plate more often than a level with four;
  that defect is real, pre-existing, and not this program's.
- **Gates that read the world, not the body** (`MAP-world-conditional-gate`,
  added at The Brattice's G3). A door that opens at moonrise or a gate the
  tide shuts is a requirement on the clock; every Brattice requirement reads
  the traverser alone, which is what keeps solvability a proof over
  `(node, keys)`. The substrate is the cheapest in the project and the proof
  is a different one; it is the first extension after this program, not a
  campaign inside it.

## 5. Provenance

Dormans, *Cyclic Generation*, in Short & Adams (eds.), *Procedural
Generation in Game Design* (2017), pp. 83–95: cycles vs. branching; graph
grammars with node/edge attributes; the fourteen cycle patterns of Fig. 9.8;
the lock-and-key attribute aside; graph-on-a-grid to tilemap (Fig. 9.9);
"two to five cycles give each level a distinct and recognizable shape."
Alexander, *A Pattern Language* (1977), patterns 98, 112, 127, 129, 131; *A
City Is Not a Tree* (1965). Series-parallel graphs: Duffin (1965); the
Kirchhoff mesh count as the cyclomatic number. The project's own prior:
`MAP-underworld-traversal-grammar`, `MAP-pattern-language-settlements`,
`CLIENT-language-not-catalogue`, `CLIENT-derived-builders`, The Hearth, The
Adit, The Stope, The Drift, The Gallery, The Latch, The Chattel. Brainstorm
and ideonomy record: `docs/superpowers/ledgers/2026-09-01-the-crosscut.md`.
