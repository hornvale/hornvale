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

#10 [G5] — Task 0 epoch grep · **No committing reader of a level cell or
level shape exists; the campaign is not an epoch** · Why: the spec §5 grep
(`Level\b|LevelCellKind|CellGrid` narrowed to `commit|Fact|ledger`, outside
`underworld_level/`, `underground.rs`, tests) returned zero lines; the
broader unnarrowed grep's hits are exactly the render/wire/movement set the
brief predicted (`session.rs`, `snapshot.rs`, `level_doc.rs`), and the
`Fact`/`ledger.commit` call sites that do exist in `session.rs`
(`turned-hostile`, `disposition-shift`, `possessed-by`,
`possession-ended`) are unrelated to level geometry — NPC hostility and
possession state, not level cells or level shape · Capture: ledger below;
Task 1 proceeds without an epoch-migration step.

## Task 0 record

**Test outcome.** Added
`underworld_level::region::tests::the_region_graph_is_a_tree_today` to the
existing `#[cfg(test)] mod tests` in
`windows/vessel/src/underworld_level/region.rs`, as the brief gives it, with
two mechanical fixes `gate-commit` required and the brief's snippet
predated: (1) `cargo fmt` wrapped the final `assert_eq!` across four lines
(a line-length fmt-check finding, no semantic change); (2) `cli/tests/suite/
claim_shape.rs` (decision 0093) default-denies any `#[test]` that loops over
seeds without a `/// claim: <shape>(...)` doc-comment tag — the brief's
snippet loops `seed_value in 0..200u64` with no tag, so the first commit
attempt failed `gate-commit` at the subfloor tier on
`claim_shape::every_seed_looping_test_in_the_repo_declares_its_claim_shape`.
Fixed by folding `claim: invariant(seed: 0..200)` into the doc comment,
matching the two pre-existing tests in this same file
(`never_exceeds_the_depth_ceiling`, `every_leaf_meets_the_minimum_span`,
both tagged `invariant(seed: 0..20)`) — the shape is the same claim: an
invariant checked over a swept seed range, not a search for an instance.
Ran:

```
cargo test -p hornvale-vessel the_region_graph_is_a_tree_today
```

Result: **PASS** (1 passed; 0 failed), across seeds `0..200`. Branch table
(brief step 2): PASS -> the spec's §1 premise stands (today's region graph
is a tree: `passages == leaves - 1`, cyclomatic number 0); proceed.

**Epoch grep (spec §5).** Command:

```
grep -rn "Level\b\|LevelCellKind\|CellGrid" --include=*.rs windows domains kernel cli \
  | grep -v "windows/vessel/src/underworld_level/\|windows/vessel/src/underground.rs\|/tests/\|#\[cfg(test)\]" \
  | grep -n "commit\|Fact\|ledger"
```

Output: **(empty — zero lines; the command's own exit status was 1, "no
match", from the final `grep`)**.

Per the controller's resolution of the pipe-yields-zero ambiguity, also ran
the broader unnarrowed grep:

```
grep -rn "Level\b\|LevelCellKind\|CellGrid" --include=*.rs windows cli \
  | grep -v "underworld_level/\|underground.rs\|/tests/"
```

Output (verbatim):

```
windows/worldgen/src/volcano.rs:214:    /// The mesh level every test here builds at. Level 5 carries edifices,
windows/worldgen/src/volcano.rs:217:    /// which a one-vertex cone cannot exercise. Level 6 is the canonical globe
windows/worldgen/src/chamber.rs:1210:/// **Level 0 is admitted by every run that exists at all**, because every
windows/worldgen/src/delve_seating.rs:506:/// Gallery's movement is over `underworld_level::Level`'s cells, never over
windows/worldgen/src/knownness.rs:279:    /// The mesh level these tests build at. Level 6 for the same reason
windows/worldgen/src/lib.rs:4270:/// open ocean), on the almanac's reference day (0.0). Level 0 — a pure
windows/lab/src/metrics.rs:10419:    /// A real (small) world's channel network. Level 5, not the canonical 6:
windows/lab/src/metrics.rs:10983:    /// Level 5 rather than the canonical grid because this is the *always*
windows/vessel/src/session.rs:935:    /// (`Vec<Level>`, one per habitation rung) plus which rung and which
windows/vessel/src/session.rs:4528:                    Some(crate::underworld_level::LevelCellKind::StairsDown) => "down",
windows/vessel/src/session.rs:4529:                    Some(crate::underworld_level::LevelCellKind::StairsUp) => "up",
windows/vessel/src/session.rs:4640:            crate::underworld_level::LevelCellKind::StairsDown
windows/vessel/src/session.rs:4642:            crate::underworld_level::LevelCellKind::StairsUp
windows/vessel/src/session.rs:5897:    ) -> crate::level_doc::SessionLevel {
windows/vessel/src/session.rs:5940:    /// answer, one band down, drawn from the exact `SessionLevel` document
windows/vessel/src/session.rs:7970:        Some(crate::underworld_level::LevelCellKind::Flooded) => "flooded",
windows/vessel/src/session.rs:7982:/// actually emits — every [`crate::underworld_level::LevelCellKind`]
windows/vessel/src/session.rs:13890:                crate::underworld_level::LevelCellKind::Floor
windows/vessel/src/session.rs:13891:                    | crate::underworld_level::LevelCellKind::Flooded
windows/vessel/src/session.rs:13904:                if level.cells.get(neighbour) == Some(crate::underworld_level::LevelCellKind::Wall)
windows/vessel/src/session.rs:13955:                    crate::underworld_level::LevelCellKind::Floor
windows/vessel/src/session.rs:13956:                        | crate::underworld_level::LevelCellKind::Flooded
windows/vessel/src/session.rs:14170:                .find(|(_, k)| *k == crate::underworld_level::LevelCellKind::StairsDown)
windows/vessel/src/session.rs:14223:                .find(|(_, k)| *k == crate::underworld_level::LevelCellKind::StairsDown)
windows/vessel/src/session.rs:14303:                .find(|(_, k)| *k == crate::underworld_level::LevelCellKind::StairsDown)
windows/vessel/src/session.rs:14424:        level: &crate::underworld_level::Level,
windows/vessel/src/session.rs:14560:                .find(|(_, k)| *k == crate::underworld_level::LevelCellKind::StairsDown)
windows/vessel/src/session.rs:14647:                .find(|(_, k)| *k == crate::underworld_level::LevelCellKind::StairsDown)
windows/vessel/src/session.rs:14694:                .find(|(_, k)| *k == crate::underworld_level::LevelCellKind::StairsDown)
windows/vessel/src/session.rs:15489:                Some(crate::underworld_level::LevelCellKind::Floor)
windows/vessel/src/session.rs:15490:                    | Some(crate::underworld_level::LevelCellKind::Flooded)
windows/vessel/src/lattice/mod.rs:241:/// underground level is a `CellGrid<LevelCellKind>`
windows/vessel/src/lattice/mod.rs:242:/// (`crate::underworld_level::CellGrid`), queried through
windows/vessel/src/level_doc.rs:4://! **Mirrors `vessel/plan/v1`, on purpose.** [`SessionLevel`] carries what
windows/vessel/src/level_doc.rs:22://! that audit measures. So the palette interns `(LevelCellKind,
windows/vessel/src/level_doc.rs:37://! handed. So [`SessionLevel::cells`] is a `Vec<`[`LevelCell`]`>` carrying
windows/vessel/src/level_doc.rs:67://! derived fresh from the live [`crate::underworld_level::Level`] and the
windows/vessel/src/level_doc.rs:78:use crate::underworld_level::{Level, LevelCellKind};
windows/vessel/src/level_doc.rs:85:/// The wire name a habitation [`Band`] carries on `SessionLevel::rung`,
windows/vessel/src/level_doc.rs:115:/// alongside [`LevelCellKind`] the way `(CellKind, Option<[u8; 3]>)` already
windows/vessel/src/level_doc.rs:134:    /// (`session.rs`) follows when it fills [`SessionLevel::marks`] (The
windows/vessel/src/level_doc.rs:152:    /// the `LevelCellKind` discriminant, never a glyph (decision 0022).
windows/vessel/src/level_doc.rs:196:    /// Index into `SessionLevel::palette`.
windows/vessel/src/level_doc.rs:204:pub struct SessionLevel {
windows/vessel/src/level_doc.rs:276:    level: &Level,
windows/vessel/src/level_doc.rs:283:) -> SessionLevel {
windows/vessel/src/level_doc.rs:303:    let mut interned: BTreeMap<(LevelCellKind, LevelVisibility), u32> = BTreeMap::new();
windows/vessel/src/level_doc.rs:323:                .expect("Level::cells is total over its extent");
windows/vessel/src/level_doc.rs:333:    SessionLevel {
windows/vessel/src/level_doc.rs:350:/// The palette entry one `(LevelCellKind, LevelVisibility)` pair becomes.
windows/vessel/src/level_doc.rs:351:fn entry_for(kind: LevelCellKind, state: LevelVisibility) -> LevelPaletteEntry {
windows/vessel/src/level_doc.rs:353:        LevelCellKind::Floor => "floor",
windows/vessel/src/level_doc.rs:354:        LevelCellKind::Wall => "wall",
windows/vessel/src/level_doc.rs:355:        LevelCellKind::Flooded => "flooded",
windows/vessel/src/level_doc.rs:356:        LevelCellKind::StairsDown => "stairs_down",
windows/vessel/src/level_doc.rs:357:        LevelCellKind::StairsUp => "stairs_up",
windows/vessel/src/level_doc.rs:369:    use crate::underworld_level::CellGrid;
windows/vessel/src/level_doc.rs:373:    /// `LevelCellKind` this module hand-maps except `StairsUp`, which
windows/vessel/src/level_doc.rs:375:    fn tiny() -> Level {
windows/vessel/src/level_doc.rs:382:        let mut cells = CellGrid::new(extent, LevelCellKind::Wall);
windows/vessel/src/level_doc.rs:385:                cells.set(Cell(x, y), LevelCellKind::Floor);
windows/vessel/src/level_doc.rs:388:        cells.set(Cell(2, 2), LevelCellKind::StairsDown);
windows/vessel/src/level_doc.rs:389:        Level {
windows/vessel/src/level_doc.rs:541:            entry_for(LevelCellKind::StairsUp, LevelVisibility::Here).kind,
windows/vessel/src/lib.rs:50:    LEVEL_SCHEMA, LevelCell, LevelExtent, LevelPaletteEntry, LevelPoint, SessionLevel, level_of,
windows/vessel/src/lib.rs:65:    Level, LevelCellKind, generate_descent, generate_descent_for_character, generate_level,
windows/vessel/src/snapshot.rs:310:/// underground now draws the level from the very `SessionLevel` document
windows/vessel/src/snapshot.rs:345:        level: Box<crate::level_doc::SessionLevel>,
windows/vessel/src/lattice/sight.rs:93:/// This is [`shadowcast`]'s body, generalized so a cave `Level` (Task 6) can
windows/scene/src/region.rs:59:    /// field — see "Level" means four things in
windows/scene/src/region.rs:233:    /// field — see "Level" means four things in
windows/almanac/src/lib.rs:262:    /// description of the sky on the almanac's reference day (0.0). Level
```

Classification, one sentence per hit-bearing file (doc-comment-only hits on
`volcano.rs`, `chamber.rs`, `knownness.rs`, `lib.rs` (worldgen),
`metrics.rs`, `lattice/sight.rs`, `scene/region.rs`, `almanac/src/lib.rs`
use "Level"/"level" in an unrelated sense — mesh subdivision level, habitation
rung count, or prose cross-reference — and are not level-cell/shape readers
at all):

- `windows/vessel/src/session.rs` — **movement.** Every `LevelCellKind`
  match queries floor/wall/flooded/stairs state to decide whether the
  agent can step onto a cell or to locate the stairs for a descent/ascent
  verb; none of it reaches `Ledger::commit` or constructs a `Fact`. The
  `Fact`/`ledger.commit` sites elsewhere in this same file
  (`turned-hostile`, `disposition-shift`, `possessed-by`,
  `possession-ended`, confirmed by a direct `grep -n "Fact\b"` on the file)
  are unrelated NPC-hostility/possession state, not level data.
- `windows/vessel/src/level_doc.rs` — **renders-or-snapshots.** Builds
  `SessionLevel`, the `vessel/level/v1` wire document, from the live
  `Level`/`LevelCellKind`; palette interning and cell classification exist
  to serialize for the client, never to commit a fact.
- `windows/vessel/src/snapshot.rs` — **renders-or-snapshots.** Carries a
  boxed `SessionLevel` inside the wire snapshot type; a pass-through, not a
  reader of level shape.
- `windows/vessel/src/lattice/mod.rs` — **renders-or-snapshots** (doc
  comment only, describing the underground level as a `CellGrid`; no
  executable reference).
- `windows/vessel/src/lib.rs` — **renders-or-snapshots** (re-exports of the
  above types; no logic).

No hit commits a fact derived from a level cell or level shape. Branch
table (brief step 3): no committing reader found -> proceed; the campaign
is **not** an epoch.

**Fixture and golden inventory.** Commands:

```
grep -rln "vessel/level/v1\|\"underground\"" clients/game/core/tests/fixtures kernel/tests windows/*/tests 2>/dev/null
grep -rn "underground\|StairsDown" docs/generated-paths.txt book/src/gallery/*.md | head
```

Output: **both empty** (no matching files/lines). Confirmed the relevant
directories are non-empty and populated (`clients/game/core/tests/fixtures`
holds three committed session JSON fixtures;
`docs/generated-paths.txt` and `book/src/gallery/*.md` both exist and are
non-trivial), so the empty result reflects absence of a match, not an empty
search set. A follow-up scan of the three committed session fixtures'
`*level*` keys found only `grid_level` (terrain/lattice grid level, unrelated
to `underworld_level::Level`) and `sea_level_m`; no underworld level content
is embedded in any committed fixture.

Branch table (brief step 4): no committed fixture embeds a level ->
**nothing to rebaseline**; Task 5 needs no `REBASELINE=1` pass for this
reason.

#11 [G5] — The `[1, 5]` lower bound was claimed "by construction" and is not ·
**Replace the count reserve with a capability invariant; the bound stays
asserted** · Why: a 400-seed × 12-vertex × 9-combination sweep (72,000 levels)
found 24 starved levels, because a cross-floor cycle anchored on ℓ spends ℓ+1's
cells; a free-cell count reserve cut that to 9 and the residue was *geometric
enclosure*, not scarcity, which no count can see. The capability test — "a level
with no realm of its own always keeps at least one feasible same-floor cycle" —
is maintained by every move that spends a level's cells, and with it the
exhaustive fallback succeeds by construction · Discarded: renegotiating the
spec's §4.2 lower bound downward (it would have made a real defect a documented
one); tuning the budget (the bound is a guard, not a prediction) · Capture:
spec §3.2 step 2 and §3.4 amended in execution and say so; decision 0566;
measured cost 29.8% → 27.3% cross-floor realm share, 0/72,000 starved.

#12 [G5] — The loop-share metric is blunt and was seen to be blunt before any
verdict existed · **The frozen metric stays frozen and may FALSIFY; a
report-only companion is printed beside it** · Why: decision 0016 — a metric's
bluntness is a finding, and redefining a preregistered readout after seeing its
behaviour is exactly what preregistration exists to prevent; the companion
(`cycle_membership_share`) is disclosed as added after the fact and is never
gated · Discarded: re-phrasing §4.1 to ask "lies on any cycle" (would have
turned a FALSIFIED headline into a PASSED one by redefinition) · Capture: spec
§4.1's disclosure paragraph; audit page; chronicle. Result: median 0.11 against
a 0.50 floor → FALSIFIED; companion 0.85.

#13 [G6] — What the density PASS actually measured · **Record the weakness
inside the decision the pass supports, not beside it** · Why: the panel medians
equal the derived budget exactly in every cell of the table on every seed,
because the grammar reaches its target on essentially every level — so the
readout confirms the budget is derived and reached and does not independently
witness that the world varies as the rule claims (the "test collapses to the
parameter" shape) · Capture: decision 0568's Consequence section; chronicle.

## Close record (G6)

Decisions minted: **0566** (a place is a graph before it is a map; the grammar
is series-parallel), **0567** (stairs pair by coordinate), **0568** (cycle
density is derived from rock and workmanship). Block 0566–0575 reserved; three
of ten used, 0569–0575 unspent.

Book: `book/src/chronicle/the-crosscut.md` plus its `SUMMARY.md` line.
Frontier sweep: `MAP-cycle-density-is-derived` → shipped (verdict and the
"measured the budget" caveat appended), `MAP-underworld-traversal-grammar`
gains the structural half, `CLIENT-semilattice-caution` gains its first
measurement (~0.29–0.31), `TOOL-underworld-embedder-unification` → shipped, and
two new rows from the followups above: `MAP-walk-ignores-the-lattice`,
`MAP-descent-carves-are-per-world`.
Retrospective: `docs/retrospectives/the-crosscut.md`.
