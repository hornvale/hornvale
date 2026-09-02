# The Crosscut — campaign ledger

Campaign: cyclic structure for the underworld (The Circuit, campaign 1).
Spec: `docs/superpowers/specs/2026-09-01-the-crosscut-design.md`. Metaplan:
`docs/superpowers/specs/2026-09-01-the-circuit-metaplan.md`. Autopilot
engaged from the first message; Nathan reviews at G3.

#1 [Q] — Is this one campaign or a program? · **A program of three; spec
the first** · Why: the request spans structure (cycles), gates (locks,
keys, valves) and inhabited reading (Alexander's gradient, the hoarder),
and each depends on the one before; precedent is The Delving (3 campaigns)
and The Bridle (5 arcs) · Discarded: one campaign doing all three —
rejected, the gate half needs The Chattel's door-as-Thing and the inhabited
half needs a `ChamberOverrides` writer that has no call site, so a single
campaign would be three epochs of scope with one gate · Ideonomy: 1 pass
(scope prompt) — scope substitution split "the cycle" into per-level /
per-run / per-system / per-world, of which per-run is the one the chamber
lattice already gives a stable key (`RunAddr`) · Capture: metaplan §3–4.

#2 [G1] — What is the structural primitive and how is it embedded? ·
**Cycle, grown series-parallel on a per-level region grid; the Adit's
partition tree retired as scaffold, its leaf carvers kept** · Why: Dormans'
graph-on-a-grid makes graph→tilemap trivial (Fig. 9.9); both shipped
embedders assert a chain (`allocate.rs`, `grow.rs`), so a general
rectangular-dual embedder would be new machinery with a residual-DoF
accounting nobody has done for cyclic graphs; series-parallel makes
planarity, reachability and realm-ownership properties by construction ·
Discarded: (i) extend `lattice/` to rectangular duals — heavy, risky,
answers a question (embed an *arbitrary* planar graph) the grammar never
asks; (ii) Brogue-style extra doors on the partition tree — cycles without
structure, so no pattern can be applied to them and nothing spans floors;
(iii) a general LHS/RHS rewrite engine with rules as data — four operations
cover every rule in the essay at ≤ a few dozen nodes per level · Ideonomy:
1 pass to convergence (`--more` tuple: dimension-identification ×
tree-finding × cross-domain re-instantiation; organons notation ×
periodic-grid; prompts intentionality, side-effect, symmetry, scope,
predictability). **Three enrichments, one overturn** — see the organon
below. The overturn: "cyclomatic number 2–5 by construction" was a
constant; the pass made it a *derived* quantity with a preregistered
ordering (spec §3.2 step 5, §4.2) · Capture: spec §2–3; registry rows
`MAP-cycle-density-is-derived`, `MAP-cross-run-cycles`,
`MAP-drop-is-a-cross-floor-valve`, `MAP-heart-is-a-hub-at-level-scale`;
`TOOL-underworld-embedder-unification` updated.

#3 [Q] — Where does the plan live? · **Plan graph in `windows/worldgen`
beside `chamber.rs`; realization in `windows/vessel/underworld_level`** ·
Why: the plan is structural world truth of the same kind as
`passages_from` and `junctions_at`, and `windows/lab` depends on `worldgen`
(and on `vessel`, so either would be measurable — the layering argument
decides, not the dependency one); decision 0022's sim-emits / client-renders
split puts the *graph* on the sim side and the *cells* one window closer to
the render · Discarded: everything in `vessel` — would make a structural
fact a property of the game seam · Ideonomy: 1 pass (tree-finding, walking
up: region → level → run → system → underworld; the plan's natural parent
is the run, whose owner is `chamber.rs`) · Capture: spec §3.1, §8.

#4 [Q] — Do the two-tier position law (0069) and the byte-identity
contracts survive changing every level's shape? · **Yes, conditionally, and
the condition is a Task 0 grep with a branch table** · Why: a `Level` is
FRAME-tier and never serialized; the Hearth's `INVENTORY` comment states
the exact condition under which a FRAME-tier change becomes an epoch ("the
day something that commits reads a chamber") · Discarded: asserting "no
committing reader exists" as a fact in the spec — the autopilot rule
forbids a tool-behaviour claim without its command-and-output pair, so the
spec carries the grep and its branches instead · Ideonomy: 1 pass
(predictability prompt) — the deterministic/stochastic substitution
confirmed the contract is on *re-derivation*, not on any stored bytes ·
Capture: spec §5.

#5 [Q] — Which stairs pairing? · **By coordinate: a stairway's two ends
share a cell** · Why: no table, no ordinal (decision 0102's "a place, never
an ordinal"), `peek_stairs`'s own doc already describes "the same physical
stairway, named from its other end"; extents grow with rank but share the
origin, so an intersection exists for the realizer to draw in — asserted,
not assumed · Discarded: an ordinal pairing (k-th down ↔ k-th up) —
re-rolls every landing the moment a stair is inserted, the id-as-offset bug
one scale up · Ideonomy: 1 pass (symmetry prompt) — the symmetric stairway
is campaign 1; its asymmetric twin, the DROP (down with no up), is the
cross-floor valve and belongs to The Brattice · Capture: spec §3.3;
registry `MAP-drop-is-a-cross-floor-valve`.

#6 [Q] — Visual companion? · **Not started; offered at G3 instead** · Why:
CLAUDE.md says to set it up during brainstorming without asking, but the
companion is a live browser session for a present human, and autopilot ran
with none present; the spec's structures are set as fenced diagrams · This
is a deviation from a standing preference and is flagged in the G3 package
rather than buried here.

## Followups (promoted into the retrospective at close)

- The `[1, 5]` cycle clip is authored and cited to Dormans; when The Plat
  gives a level residents, revisit whether density should also read
  population (a city breathes more than a lair).
- `MAP-underworld-viewport`: fifteen regions per level makes the missing
  camera-follow bite harder. Pre-existing; not this campaign's.
- The surface-building embedder (`lattice/`) keeps its chain; The Precincts
  should read spec §3.4 before deciding whether districts want this grid.

## The ideonomy organon (G1)

### Notation: the plan as a series-parallel expression

```
  P(x, y)     parallel: two paths between the same two nodes  -> a cycle
  S(x, y)     series:   one path after another                -> lengthening
  e           an edge on one floor
  v(e)        an edge that changes floor (a stairway)

  a plain corridor            S(e, e, e)
  Dormans' simple cycle       P( S(e,e), S(e,e,e) )                LongShort
  a nested cycle              P( S(e, P(e, S(e,e))), S(e,e,e,e) )
  a cross-floor cycle         P( S(e,e), S(v(e), e, e, v(e)) )     the return
                                                                  is downstairs
  required slots the notation forced:
    - every v(e) needs a cell shared by both floors      -> spec 3.3
    - every P needs its two paths node-disjoint          -> spec 3.2 step 2
    - the outermost S needs an entrance and a terminus   -> the spine
  inexpressible in the notation (so out of scope, on purpose):
    - a shortcut joining two different P's                (K4; not SP)
    - an edge into another run                            (junction)
```

### Periodic grid: length class x floor span

```
  rows: Dormans' four length classes (+ the degenerate hub)
  cols: how many floors the cycle touches

                | same floor            | two floors                | whole run
  --------------+-----------------------+---------------------------+---------------------
  long a/long b | two alternative paths | Zelda: down one stair,    | the run's spine and
                | (Dormans)             | along, up another         | its long way back
  long a/short b| hidden shortcut       | PREDICTED: a chute --     | PREDICTED: the
                | (Dormans)             | the short way is the drop | delving's shaft
  short a/long b| lock-and-key cycle    | PREDICTED: key downstairs,| PREDICTED: the key
                | (Dormans)             | lock upstairs             | at the bottom
  short a/short | patrolled cycle       | PREDICTED: a stair pair   | (empty: a run-long
    b           | (Dormans)             | one region apart -- the   |  cycle cannot be
                |                       | landing hall              |  short)
  hub (b = 0)   | forbidden at ROOM     | PREDICTED: the shaft with | (empty)
                | scale (Hearth's       | a landing on every floor  |
                | anti-hub); at LEVEL   | -- Alexander 133,         |
                | scale it is Alexander | staircase as a stage      |
                | 129, the heart        |                           |

  Every "PREDICTED" cell is a pattern The Brattice or The Plat can name.
  The two empties are structural, not gaps.
```

### Cross-domain re-instantiations (what other fields do about the same shape)

- **Electrical circuits.** Independent meshes = cyclomatic number; a valve is
  a diode, a lock a switch, a dangerous lock a resistor. Series-parallel
  networks are the tractable class, and nesting a cycle is adding a mesh
  that shares an edge. -> the grammar is SP, and 4.2's metric is the mesh
  count.
- **Mine ventilation.** A working must have loops: intake and return. A
  dead-end drift is the unventilated one. -> `worked` raises density; the
  campaign name.
- **Karst hydrology.** Dissolution finds many routes; a lava tube is one
  conduit. -> `base(CaveKind)`.
- **Vascular anastomosis.** Loops so a blockage does not kill tissue;
  collateral circulation is the hidden shortcut; vessels widen with flow. ->
  The Plat: corridor width from traffic; The Brattice: the shortcut pattern.
- **Processional liturgy.** A circuit with stations and a one-way direction
  enforced socially. -> The Plat's normative gates (`MAP-normative-terrain`).

### Dimensions surfaced (and where each went)

- intentionality: designed / rule-authored (here) / worn (desire paths, rung 4)
- side-effect swapped with main effect: cycle = residents' circulation first
  -> the program keystone
- symmetry: stairway (symmetric, here) vs drop (valve, The Brattice)
- scope: level / run (here) / system (junctions, captured) / world
- predictability: recognizable vocabulary, unrecognizable instance (the
  Hearth's "houses of one people look alike")

## Post-G3 entries (lead the G6 digest)

#7 [G4] — The G3 draft keyed the plan to a `RunAddr` refining
`passages_from`; the walked descent is not that object · **Re-key to the
five-rung descent `Underground::enter` builds under `(seed, vertex)`** ·
Why: `enter` generates one level per habitation rung and never reads
`levels_in_branch`, `passages_from` or a branch (`MAP-chamber-occupancy`
records that the lattice's edge half has no walking consumer); a plan per
run would describe a place nobody stands in, and wiring the walk to the
lattice is its own campaign · Discarded: widening this campaign to do the
wiring — a second epoch of scope with one gate · Ideonomy: 1 pass (scope
prompt re-run against the code rather than the docs) — the per-run /
per-descent substitution is exactly where the draft's reading and the
code's reality diverged · Capture: spec §3.1 rewritten and says so;
followup below for the wiring gap.

#8 [G4] — Readout sample · **Three-seed panel over every cave-bearing
vertex, as `hornvale underworld` does; drop the 60-seed sweep from the
committed page** · Why: a committed artifact must regenerate in seconds
(the lattice page precedent), and 3 seeds × every cave vertex is ~3,800
descents against the sweep's 60 · Ideonomy: 1 pass (scope) — the sweep's
value was terrain variety, which every-vertex already supplies within a
seed · Capture: spec §4.

#9 [G4] — Acceptance 2's "scripted fixture" · **A session test driving the
verbs, not a committed transcript** · Why: a gallery page adds two
generated paths and a regeneration step for one acceptance criterion; the
verbs are what the criterion cares about · Capture: spec §7.

## Followups (added post-G3)

- **The walk and the lattice disagree.** `Underground::enter` builds one
  level per rung; The Stope/Drift's runs, branches, entrances and junctions
  have no walking consumer. A campaign that wires `delve` to the lattice
  would give the plan a `RunAddr` again; until then `MAP-chamber-occupancy`
  is the standing record.
- `generate_descent_for_character` is seeded from the WORLD seed with no
  vertex, so every cave system in a world carves identical level interiors
  (only kind, depth and water differ). The plan is per-vertex; the carves
  are not. Pre-existing; not this campaign's to fix, and worth a row.
