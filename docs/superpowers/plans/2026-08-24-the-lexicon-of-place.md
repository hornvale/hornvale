# The Lexicon of Place — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> superpowers:subagent-driven-development to implement this plan
> task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make the spatial vocabulary say what it means — a `Vertex` is a
point, a `Facet` is a patch — everywhere in the code, while every serialized
spelling keeps saying "room" and "cell" forever, and a guard keeps the old
word from growing back.

**Spec:** `docs/superpowers/specs/2026-08-24-the-lexicon-of-place-design.md`.
Read it first, §4 especially. **This plan departs from the spec in two
places, both on evidence the spec did not have** — see "Spec deltas" below.

**Tech Stack:** Rust 2024, whole workspace plus `clients/` and
`tools/type-audit`. No new dependencies. No behaviour change anywhere: the
campaign's headline evidence is that every committed byte is unmoved.

**Decision block:** 0246-0255 (reserved 2026-08-24; main ceiling was 0230).

---

## Spec deltas

Both were settled by Nathan on 2026-08-24 and are ledgered in
`.superpowers/sdd/decision-ledger.md` (#4, #5). They must be promoted into the
spec as part of Task 1.

**D1. The vertex type is `Vertex`, not `Node`.** `Node` is taken four ways in
this repo and adding a fifth is the defect class this campaign removes:

```
windows/chronicle/src/config.rs:28   pub struct NodeId(pub u32)   a graph node, ~36 uses, re-exported
domains/terrain/src/branch.rs:448    struct Node                  a river-network node
domains/astronomy                    "node line", "node longitude"  an ORBITAL node
clients/                             node, createTextNode         DOM / Node.js
```

`Vertex` is held by nothing (one private `VertexGrid` in
`domains/terrain/src/channel.rs:534`), is the word the spec's own §2 diagnosis
uses, already appears in 231 doc-comment lines, and pairs with `Facet` as
ordinary mesh vocabulary — a vertex is a point, a facet is a patch, which is
exactly the distinction §2 says nothing currently signals.

**D2. The sweep is total, and it ends in a guard.** The spec renames the
*type*; this plan renames the *word*. Measured on `e2453a63b`:
**447 distinct `cell`-bearing identifiers, 16,201 occurrences in `*.rs`** —
bare `cell` (8,014) and `cells` (2,499) dominate, most of them doc prose.
Renaming only the type would leave `VertexMap<T>` documented as "a value per
cell", which is worse than not renaming at all.

The guard is not decoration. "cell" arrived here by **convergent emergence** —
every author reached for it independently, out of the GIS/raster convention
the spec §2 names — so a one-time rename decays. Only a check holds it.

---

## Global Constraints

- **No `HashMap`/`HashSet`** — `BTreeMap`/`BTreeSet`/`Vec` only. Float sorting
  uses `total_cmp`. Enforced by `clippy.toml` `disallowed-types`.
- **No wall-clock time.** No new dependencies (`serde`, `serde_json`, `libm`).
- **Every crate sets `#![warn(missing_docs)]`.** Renaming a `pub` item moves
  `docs/audits/type-audit-report.md`; regenerate it in the same commit.
- **Rust edition 2024**; `cargo fmt` is the final step before every commit.
- **`git grep -E` has no `\b`.** Use `-P`. An `-E` count silently
  under-reported this campaign's own blast radius by 1.8x before it was caught.
- **Run the suite once, capture, then grep** — never re-run to read a second
  line. Redirect to a file and grep the file.
- **Assert the branch before every commit:**
  `git branch --show-current | grep -qx 'campaign/the-lexicon-of-place' || exit 1`
- **Absorb `main` at every stage boundary** (`make sluice-stage`). This
  campaign touches kernel → domains → windows → cli → clients, so it conflicts
  with every live branch; the board currently shows hold-offs from
  `campaign/the-escapement`, `campaign/the-confidant` and `campaign/the-hand`
  on `windows/vessel/`. Small absorptions, land fast.
- **Two regeneration paths, and `make rebaseline` is only one of them.**
  `windows/vessel/tests/fixtures/`, `cli/tests/fixtures/` and
  `windows/worldgen/tests/fixtures/` are byte-goldens written ONLY by
  `make rebaseline-goldens` (`REBASELINE=1`). If this campaign is correct,
  **neither command should move a single byte** — see Task 9.

---

## THE FREEZE — read before touching anything

Every string below is a save-format, wire, or committed-data contract. The
**constant name** may be renamed; the **string value** may not. After this
campaign the code says `Facet` while the label says `"room/face"`, and that
mismatch is correct and permanent.

**Seed-derivation stream labels** (`stream_labels()` publishes these into a
drift-checked manifest, so changing one reddens the drift check — partial
cover, and the only cover that already exists):

```
kernel/src/streams.rs          ROOM_FACE       = "room/face"
kernel/src/streams.rs          ROOM_CHILD      = "room/child"
windows/vessel/src/streams.rs  ROOM_FURNISHING = "room/furnishing/v1"
windows/vessel/src/streams.rs  ROOM_CHAMBERS   = "room/chambers/v1"
windows/vessel/src/streams.rs                    "room/layout/v1/rectilinear"
windows/vessel/src/streams.rs                    "room/layout/v1/grown"
windows/vessel/src/streams.rs                    "room/layout/v1/anchors"
domains/settlement/src/lib.rs  CELL_ID         = "cell-id"   (a predicate name)
```

**Epoch keys** — `kernel/src/world.rs`: `"room/furnishing" => "v1"`,
`"room/layout/rectilinear" => "v1"`.

**The knowledge-key prefix** `"room/"` — written at
`windows/vessel/src/knowledge.rs:110`, read at
`windows/vessel/src/purview.rs:146`. **Two literals, two files, nothing
asserting they agree.** Task 2 closes this.

**Serialized JSON keys**, counted in committed artifacts and fixtures:
`"room"` ×306, `"cell"` ×30, `"cells"` ×12, across
`book/src/gallery/scene-surrounds-seed-42.json`,
`book/src/reference/locale-seed-42.json`,
`clients/game/core/tests/fixtures/`, `windows/scene/tests/fixtures/` and
`windows/vessel/tests/fixtures/`. `scene/surrounds/v2` is a **cross-repo
contract** (additive-or-versioned only).

**Committed data columns** — the first two are census goldens, so moving one
is a census-refresh carve-out requiring Nathan's explicit authorization:

```
per-cell-diversity        book/src/laboratory/generated/the-census/rows.csv
cold-built-room-share     ...and census-of-the-meeting, and 8 lab fixtures
cell                      book/src/laboratory/generated/earth-mask-l6/rows.csv
cells_occupied            windows/worldgen/tests/fixtures/occupancy.csv
land_cells                windows/worldgen/tests/fixtures/repose-exposure.csv
```

**`RegionScene.level`** (`windows/scene/src/region.rs:240`) — a serialized
field of `scene/tiles-region/v1`. The spec says `level` means three things; it
means **four**, and this fourth one is a cube-face quadtree depth on a wire
contract. Frozen, and it earns a glossary row.

### The serde rule (decision rule, not a prediction)

Rename every Rust field. Then, for each renamed field on a type deriving
`Serialize`/`Deserialize` whose wire name appears above, add
`#[serde(rename = "<old wire name>")]`.

Do it this way round — rather than leaving those fields spelled `cell` — for
the reason §5.1 gives about the `"room/"` prefix: an attribute is a **visible**
tripwire, while a silently-old field name is exactly what the next sweep
renames without noticing. Task 9's byte-identity check is what proves the set
is complete; a missed attribute moves a committed golden and lights up.

---

## THE ROSTER

### Rename — the vertex sense (a POINT, where fields are sampled)

Cross-crate visible; all of these move in one commit (Task 3).

```
CellId                 -> Vertex
CellMap<T>             -> VertexMap<T>
NearestCellIndex       -> NearestVertexIndex
CellFeatureIndex       -> VertexFeatureIndex     domains/terrain/src/landscape.rs
CellBoundary           -> VertexBoundary         domains/terrain/src/boundaries.rs
CellRoute<'a>          -> VertexRoute<'a>        domains/topology/src/route.rs
CellWeight             -> VertexWeight           windows/locale/src/lib.rs  (serde rule applies)
Geosphere::cells()     -> vertices()
Geosphere::cell_count()-> vertex_count()
cell_at, nearest_cell, as_cell_map, into_cell_map, by_cell, raw_cell,
variant_at_cell, cell_share, cell_catchment, cell_edge, cell_spacing,
land_cells, run_cells, delta_cells, cave_cells, anchor_cells, barrier_cells,
atoll_cells, ... (the full 447-identifier enumeration is Task 5's input)
```

### SPARE — the grid sense (an AREA; "cell" is CORRECT here) and unrelated

Renaming any of these is a defect. They go in the guard's allowlist.

```
CellKind            windows/vessel/src/lattice/mod.rs      a building-interior grid square
LevelCellKind       windows/vessel/src/underworld_level/   an underworld-level grid square
ParadigmCell        domains/language/src/paradigm.rs       a cell of an INFLECTION TABLE
realize_paradigm_cell, cells: &BTreeMap<String, ParadigmCell>
SurroundsCell       windows/scene/src/surrounds.rs         a chart grid square
ChartCell           clients/game/core/src/schema.rs        the client's mirror of it
chart_cell*, Cell (clients/game/core/src/cell.rs)
RefCell             std
CellularCave, cellular, cancellation, excellent            substring false positives
```

### Rename — the face sense (a PATCH, an area you can occupy)

```
RoomAddr      -> Facet
RoomId        -> FacetId
RoomAddrError -> FacetError
RoomIdError   -> FacetIdError
```

### Rename — refinement depth

```
Geosphere::level field + level() -> depth field + depth()
windows/vessel/src/band.rs       -> windows/vessel/src/depth.rs
```

**NOT renamed:** `ChamberAddr.level` (The Drift: "a level is a screen-filling
map, not a storey" — a different and correctly named thing) and
`RegionAddr.level` / `RegionScene.level` (see the freeze).

---

## Task 1 — Promote the spec deltas; ledger the shape

**Files:** `docs/superpowers/specs/2026-08-24-the-lexicon-of-place-design.md`

- [ ] Add a `## 0. Deltas from the approved draft` section recording D1 and D2
      above, with the four `Node` collisions cited by file and line.
- [ ] Correct §3's scope table to `Vertex` and to the measured counts.
- [ ] Correct §3's "`level` currently means three things" to four, naming
      `RegionScene.level` and its wire contract.
- [ ] Extend §5 with the four freeze classes it omits (metric names, CSV
      columns, JSON keys, `RegionScene.level`).
- [ ] Commit. `git branch --show-current` asserted first.

**Success:** the spec a future reader finds describes what was built.

---

## Task 2 — The `"room/"` prefix guard (spec §5.1; do this FIRST)

The spec calls this a first-class deliverable, and it must land **before** the
rename so it is protecting during it.

**Files:** `windows/vessel/src/knowledge.rs` (+ its test module)

- [ ] **Capture the behavioural red first.** Before writing the guard, use
      `scripts/mutate.py` to change the `format!("room/{}", ...)` literal in
      `knowledge.rs:110` to something else, run the vessel crate's suite, and
      RECORD the result. Restore from a `/tmp` copy — **never
      `git checkout --`**, which would also revert your new test.
      - The spec predicts nothing fails. If something DOES fail, say so and
        name it: the gap is smaller than §5.1 claims and the guard is narrower.
- [ ] Write a test co-located with the writing side asserting the exact
      literal, and **naming `windows/vessel/src/purview.rs` by path** in its
      failure message and doc comment as the reader that depends on it. This is
      the remedy shape from the 2026-08-23 delve-literal fix; follow it.
- [ ] Re-run the mutation. The new test must go RED. A green here means the
      guard is vacuous — do not proceed until it fails.
- [ ] Restore, run the crate suite, commit.

**Success:** a recorded RED from the mutation with the guard in place, and a
recorded result from the same mutation without it.

---

## Task 3 — The mechanical rename, workspace-wide, one commit

Everything cross-crate-visible moves together; a partial rename does not
compile, so this cannot be split.

- [ ] Apply the vertex-sense, face-sense and depth renames from THE ROSTER
      across `*.rs` in the workspace, `clients/`, and
      `tools/type-audit/src/extract.rs`.
- [ ] **Verify the SPARE list survived**: after the sweep,
      `git grep -cP '\b(CellKind|LevelCellKind|ParadigmCell|SurroundsCell|ChartCell|RefCell)\b'`
      must match its pre-sweep count. Record both numbers.
- [ ] **Verify no frozen string moved**:
      `git diff -U0 | grep -P '^[+-].*"(room|cell)[/"-]'` must show only
      constant-name changes, never value changes. Record the output.
- [ ] Apply the serde rule to every renamed serialized field.
- [ ] `cargo fmt`, `cargo clippy --workspace --all-targets -- -D warnings`,
      then a workspace check.
- [ ] `make gate-commit`. Commit.

**Success:** green gate-commit; SPARE counts unmoved; no frozen value in the
diff.

---

## Task 4 — Regenerate, and prove nothing moved

- [ ] `make rebaseline`, then
      `git diff --exit-code -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$')`
- [ ] `make rebaseline-goldens`, then `git status --short`.
- [ ] **Branch table, not a prediction:**
      - Only `docs/audits/type-audit-report.md` moved → expected (the type
        names it reports changed). Commit it in this commit.
      - `book/src/gallery/`, `book/src/reference/`, `book/src/laboratory/`,
        `book/src/domesday/`, `clients/game/core/tests/fixtures/`,
        `windows/vessel/tests/fixtures/`, `windows/worldgen/tests/fixtures/`
        or `cli/tests/fixtures/` moved → **STOP.** A rename moved a byte;
        find which frozen string changed before doing anything else.
      - Nothing moved at all → the type-audit report did not regenerate.
        Check the redirect actually ran; a bare command writes nothing and the
        following drift check then reads clean.

**Success:** exactly one artifact moved, and it is the type-audit report.

---

## Task 5 — The prose and local-identifier sweep, per crate

Parallelizable: one subagent per crate, since everything remaining is
crate-local. **Input:** the 447-identifier enumeration
(`git grep -hoP '\b\w+\b' -- '<crate>/**/*.rs' | grep -i cell | sort | uniq -c`).

Crates, in rough order of size: `windows/worldgen`, `windows/vessel`,
`domains/terrain`, `clients/game`, `domains/climate`, `windows/lab`,
`kernel`, `cli`, `windows/locale`, `windows/scene`, `domains/demography`,
`windows/almanac`, `windows/hearsay`, `domains/topology`, `domains/language`,
`domains/settlement`, `domains/history`, `domains/paleoclimate`,
`windows/explain`, `tools/`.

For each crate:

- [ ] Rename crate-local identifiers of the vertex sense.
- [ ] Rewrite doc comments and comments. **This is judgement, not
      substitution** — a doc saying "the room's cells" is saying something
      true about a facet and its vertices and must survive still saying it.
- [ ] Leave every SPARE-list name and every frozen string alone.
- [ ] Leave test *names* alone where a rename would change the sub-floor
      roster (`docs/timings/subfloor-roster.tsv` selects by EXACT name; a
      stale id drops tests from the commit gate silently). If a test name must
      change, update the roster in the same commit.
- [ ] `cargo fmt` + `cargo clippy -p <crate> --all-targets -- -D warnings` +
      the crate's own test run.

**Success:** `git grep -ciP '\bcells?\b' -- '<crate>/**/*.rs'` reaches zero
except for allowlisted grid-sense names and frozen strings.

---

## Task 6 — `level` → `depth`, and the band module

- [ ] `Geosphere`'s `level` field and `level()` accessor → `depth` / `depth()`,
      with every caller and doc line.
- [ ] `git mv windows/vessel/src/band.rs windows/vessel/src/depth.rs`; update
      `mod` declarations, imports, and the module doc (which currently
      explains that it holds no `Band` type — that explanation gets simpler,
      not deleted).
- [ ] **Leave `ChamberAddr.level` and `RegionAddr`/`RegionScene.level`
      alone.** Add a one-line doc comment to each saying which "level" it is
      and pointing at the glossary — the collision is kept deliberately, so it
      should be labelled.
- [ ] `make gate-commit`. Commit.

---

## Task 7 — The re-accretion guard

The durable half. Same three-valued shape as `tropes check`, the timings
baseline and type-audit's `waiver(...)`: it fails on **novelty**, not on
existence, so it can be green on day one and still bite.

**Files:** a new test under `cli/tests/suite/` (the home of the workspace-wide
enforcement tests).

- [ ] Scan `*.rs` across the workspace and `clients/` for `\b[Cc]ells?\b` and
      `[Cc]ell` inside identifiers.
- [ ] Pass a match if it is in the allowlist: a SPARE-list name, a frozen
      string, or a line carrying an explicit waiver comment with a reason
      (reasonless is a failure, per the seam-guard precedent).
- [ ] Fail on anything else, with a message that says *why* — a cell is an
      area, a `Vertex` is a point — and points at the glossary page.
- [ ] **Prove it is not vacuous:** add a vertex-sense "cell" to a real file,
      run the test, record the RED, remove it. A guard that has never failed
      is not evidence.
- [ ] **Name the direction it enforces in its doc comment.** It asserts
      "no un-allowlisted vertex-sense cell"; it is structurally blind to a
      SPARE-list entry that later stops being grid-sense. Say so.

**Success:** a recorded RED against a deliberate reintroduction.

---

## Task 8 — The glossary (spec §7)

**Files:** a new page under `book/src/`, linked from `SUMMARY.md`.

- [ ] Fix the words: `Vertex` (point) and `Facet` (patch), one icosphere, two
      indexings; `Band` (the cave rungs, kernel, ratified 2026-08-23) vs
      `Stratum` (position in a column); `Medium` / `Aperture` / `Chamber` /
      `Anchor`; `Depth` (mesh refinement) vs the two `level`s that remain.
- [ ] State the frozen-spelling rule **with its reason**, and list what is
      frozen. A reason is a tripwire; a conclusion is a wall.
- [ ] Explain why the archive says "room" and "cell" and must not be swept —
      those are records of what was true when written.
- [ ] Note that "Room" is retired as a word and must not be reused for
      anything (spec §4).
- [ ] Record §8's deferred `Place = (Facet, Depth)` idea as an idea-registry
      row so it is not rediscovered from scratch.
- [ ] `mdbook build book`.

---

## Task 9 — Byte-identity: the campaign's headline evidence

- [ ] The full workspace suite plus doctests, captured to a file and grepped.
- [ ] `make vessel-check`, `make world-check`, `make game-check` — the clients
      are outside the workspace and no local rung builds them.
- [ ] `make rebaseline && make rebaseline-goldens`, then a full
      `git status --short`. **Only `docs/audits/`, `docs/digest/` and the book
      pages this campaign authored may differ.**
- [ ] `make seam-guard` (nothing runs it for you since decision 0148; it needs
      a clean tree of TRACKED files).

**Success:** a rename that changed a stream label would move every name in
every world. Nothing moved.

---

## Task 10 — Definition of Done

- [ ] Decision record `docs/decisions/0246-<slug>.md`: the frozen-spelling
      rule and its guard. Use the reserved block; minting outside it passes
      every mechanical check and collides later.
- [ ] A second record if D1 warrants one (`Vertex` over `Node`, with the four
      collisions) — 0247.
- [ ] Chronicle entry `book/src/chronicle/the-lexicon-of-place.md`.
- [ ] Freshness sweep: any chapter whose prose describes `RoomAddr`/`CellId`
      by name. **Chronicles of PAST campaigns are historical records — do not
      sweep them**; sweep chapters that describe the CURRENT system.
- [ ] Re-score `book/src/open-questions.md` if this moved a Confidence
      Gradient bet.
- [ ] Retrospective `docs/retrospectives/the-lexicon-of-place.md`, with the
      followup register promoted into it.
- [ ] `make rebaseline`, drift check, commit.
- [ ] Authored `Sluice-Headline:` trailer in the range — a merge REFUSES
      without it; same trailer block as `Claude-Session`, no blank line
      between.
- [ ] `make sluice BRANCH=campaign/the-lexicon-of-place REF=<full-sha>`.
