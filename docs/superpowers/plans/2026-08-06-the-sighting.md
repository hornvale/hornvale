# The Sighting Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Join Hornvale's two fine spatial layers — embed the relational
`Interior`'s anchors into `Lattice` cells — so creatures stand somewhere
drawable, sight through apertures narrows what a client is shown, and the
floor plan becomes inhabited.

**Architecture:** Anchors are discovered; cell geometry is invented. A new
`anchor_cells` derivation places each chamber's anchors at cells, *faithfully*
(adjacent anchors get a passable path between them). `liveness::Occupancy`'s
`(RoomAddr, AnchorId)` resolves through it into `lattice::Occupancy`. Symmetric
integer shadowcasting narrows the snapshot's `sensed` channel sim-side. The
client draws creatures and unlit cells.

**Tech Stack:** Rust (edition 2024, `serde`/`serde_json`/`libm` only),
TypeScript on Deno 2.9.2 (pinned).

**Spec:** `docs/superpowers/specs/2026-08-06-the-sighting-design.md`

## Global Constraints

- **No new dependencies** — `serde`, `serde_json`, `libm` (decisions 0004/0041).
- **No `HashMap`/`HashSet`** — `BTreeMap`/`BTreeSet`/`Vec` only, enforced by
  `clippy.toml` (decision 0005).
- **No wall-clock time.** `std::time::Instant`/`SystemTime` are banned
  workspace-wide **including test and example code**. The bench task needs a
  scoped `#[allow(clippy::disallowed_types)] // benchmark harness` on the
  statement naming the type, never on the function.
- **`#![warn(missing_docs)]`** — every public item, field and enum variant.
- **`type-audit:` tags on every primitive at a `pub` boundary**, form
  `bare-ok(class: field)` — **class first**. Tag `return` on any `pub fn`
  returning a bare primitive. `make gate` runs both halves; do **not** add a
  manual regen step to a task, but **do** regenerate
  `docs/audits/type-audit-report.md` when a task adds `pub` items, and commit
  it in the same commit.
- **No floats in the FOV path.** Integer arithmetic only, matching
  `kernel/src/astar.rs`'s `u64` costs and for the same reason.
- **Names verified free before use:** `anchor_cells`, `AnchorCells`,
  `shadowcast` are unused. **Do NOT introduce a type named `Visibility`** —
  `domains/astronomy` owns it for atmospheric clarity (`Visibility::CLEAR`),
  and reusing it would be a semantic collision.
- **Rust edition 2024. `cargo fmt` as the final action before every commit.**
- **Two gates.** `make gate` cannot see `clients/`; client tasks run
  `make vessel-check`. Budget `timeout: 3600000` for any gate.

## File Structure

**Create:**
- `windows/vessel/src/lattice/anchor_cells.rs` — the join. Pure: `Interior` +
  `Lattice` + chamber index + seed in, `BTreeMap<AnchorId, Cell>` out.
- `windows/vessel/src/lattice/sight.rs` — symmetric integer shadowcasting.
  Pure: `Lattice` + origin + radius in, `BTreeSet<Cell>` out.
- `clients/vessel/src/pane_plan_marks_test.ts` — the client's mark tests.

**Modify:**
- `windows/vessel/src/lattice/mod.rs` — declare the two new modules.
- `windows/vessel/src/plan.rs` — add `marks` to `vessel/plan/v1`.
- `windows/vessel/src/session.rs` — populate occupancy, narrow `sensed`.
- `clients/vessel/src/pane_plan.ts` — draw marks and unlit cells.
- `clients/vessel/src/pane_chart.ts` + `snapshot.rs` — the `v1`→`v2` sweep.
- `windows/vessel/examples/turn_cost.rs` — the ABI measurement.

**Why two new files rather than growing `lattice/mod.rs`:** it is already the
crate's densest module, and both new pieces are pure functions with no shared
state — they test without building a world, which is the difference between a
millisecond test and a two-second one.

---

### Task 1: Measure a turn through the wasm ABI

Owed since The Action Clock. `CLIENT-four-clocks` records the turn-clock
re-measurement as not done and closes with "Re-measure, do not extrapolate."
Every browser figure in this repo — including the whole performance discussion
that preceded this campaign — is native × an extrapolated 3.6–3.8× ratio. This
task replaces the extrapolation with a number, **before** the campaign adds
per-turn work.

**Files:**
- Modify: `windows/vessel/examples/turn_cost.rs` (doc comment only)
- Create: `clients/vessel/wasm/turn_bench.mjs`

**Interfaces:**
- Consumes: the `hv_*` ABI — `hv_start(seed) -> i32`, `hv_in_ptr()`,
  `hv_handle(len) -> i32`, `hv_out_ptr()`, `hv_out_len()`,
  `hv_snapshot_ptr()`, `hv_snapshot_len()`. Read
  `clients/vessel/wasm/drive.mjs` first — it already drives this ABI from Node
  and is the working template.
- Produces: printed figures. Nothing depends on it.

- [ ] **Step 1: Read the existing driver**

Read `clients/vessel/wasm/drive.mjs` end to end. It loads the wasm, calls
`hv_start`, writes a verb into `hv_in_ptr()`, calls `hv_handle`, and reads
`hv_out_ptr()`/`hv_out_len()`. Your bench is that file's loop with timing
around it. Do not invent a second way to drive the ABI.

- [ ] **Step 2: Build the wasm**

Run: `make wasm-vessel`
Expected: writes `book/src/gallery/vessel.wasm`, ~1.5 MB. **This is required
and easy to skip** — the `.wasm` is git-ignored (decision 0052), so a fresh
worktree has none, and a stale one silently measures old code.

- [ ] **Step 3: Write the bench**

`clients/vessel/wasm/turn_bench.mjs`. Use the **same fixed verb sequence** as
`windows/vessel/examples/turn_cost.rs`'s `SEQUENCE` constant — read it from
that file and copy it verbatim, so the native and ABI readings are comparable.
Time, with `performance.now()`:

- `hv_start(42)` — genesis
- each `hv_handle` call — the turn
- each `hv_snapshot_ptr`/`hv_snapshot_len` read plus the `TextDecoder` decode
  — the snapshot the client actually pays for

Report medians over 5 runs, and **split by the same three verb classes**
`turn_cost.rs` uses (moving / day-advancing / neither). Print the snapshot byte
counts per band too.

- [ ] **Step 4: Run it**

Run: `node clients/vessel/wasm/turn_bench.mjs book/src/gallery/vessel.wasm 2>&1 | tee /tmp/hv-abi-turn.txt`
Expected: figures. Nothing asserts.

- [ ] **Step 5: Record the comparison in `turn_cost.rs`'s doc comment**

Append a `## Measured — through the wasm ABI` section with date, box, and the
figures beside the native ones already recorded there. **State the real ratio
as a number.** If it differs materially from the repo's assumed 3.6–3.8×, say
so plainly — that is a finding about every extrapolated figure in the repo, not
a footnote.

- [ ] **Step 6: Commit**

```bash
cargo fmt
git add windows/vessel/examples/turn_cost.rs clients/vessel/wasm/turn_bench.mjs
git commit -m "bench(sighting): measure a turn through the wasm ABI, not through a ratio"
```

---

### Task 2: `anchor_cells` — the join

The campaign's keystone, and pure: no session, no world build.

**Files:**
- Create: `windows/vessel/src/lattice/anchor_cells.rs`
- Modify: `windows/vessel/src/lattice/mod.rs` (add `pub mod anchor_cells;`)

**Interfaces:**
- Consumes: `crate::interior::{Interior, AnchorId}`, `crate::lattice::{Cell,
  CellKind, Lattice}`, `hornvale_kernel::Seed`.
- Produces, and Tasks 3–5 depend on these exact names:
  - `pub fn anchor_cells(interior: &Interior, lattice: &Lattice, chamber: usize, seed: Seed) -> BTreeMap<AnchorId, Cell>`
  - `pub fn is_faithful(interior: &Interior, lattice: &Lattice, placement: &BTreeMap<AnchorId, Cell>) -> bool`

- [ ] **Step 1: Read the two layers before writing**

Read `windows/vessel/src/interior/anchor.rs` (the `Interior` API: `ids()`,
`anchor(id)`, `neighbors(a)`, `walkable_neighbors(a)`, `relation(a, b) ->
Rcc8`) and `windows/vessel/src/lattice/mod.rs` (`Lattice`, `CellKind::serves`,
`Cell`, `neighbours`). **Note `CellKind::serves(chamber)` — it is how you
find the cells belonging to one chamber, and a threshold serves two.**

- [ ] **Step 2: Write the failing property tests**

The faithfulness property is the campaign's central invariant, so it is a
**property test over generated structures**, not an example:

```rust
#[cfg(test)]
mod tests {
    use super::*;

    /// Faithfulness, as §2.2 of the spec defines it: anchors adjacent in the
    /// interior are placed at cells with a passable path between them that
    /// crosses no third anchor's cell.
    ///
    /// A property over every structure the generator produces, not one
    /// example — an embedding can be faithful for a two-anchor room and
    /// scatter a six-anchor one, and only the sweep sees that.
    #[test]
    fn every_placement_is_faithful() {
        for n in 1..=crate::structure::MAX_CHAMBERS {
            for seed in 0u64..64 {
                let (interior, lattice, chamber) = fixture(n, Seed(seed));
                let placed = anchor_cells(&interior, &lattice, chamber, Seed(seed));
                assert!(
                    is_faithful(&interior, &lattice, &placed),
                    "n={n} seed={seed}: adjacent anchors were placed without a \
                     passable path between them"
                );
            }
        }
    }

    #[test]
    fn every_anchor_is_placed_exactly_once() {
        let (interior, lattice, chamber) = fixture(2, Seed(7));
        let placed = anchor_cells(&interior, &lattice, chamber, Seed(7));
        assert_eq!(placed.len(), interior.ids().len(), "not every anchor got a cell");
        let mut cells: Vec<Cell> = placed.values().copied().collect();
        cells.sort();
        cells.dedup();
        assert_eq!(cells.len(), placed.len(), "two anchors share one cell");
    }

    #[test]
    fn every_placed_cell_serves_this_chamber() {
        let (interior, lattice, chamber) = fixture(3, Seed(11));
        let placed = anchor_cells(&interior, &lattice, chamber, Seed(11));
        for (id, cell) in &placed {
            let kind = lattice.cells.get(cell).expect("placed inside the extent");
            assert!(
                kind.serves(chamber),
                "anchor {id:?} was placed at {cell:?}, which does not serve chamber {chamber}"
            );
        }
    }

    #[test]
    fn the_placement_is_deterministic() {
        let (interior, lattice, chamber) = fixture(2, Seed(3));
        let a = anchor_cells(&interior, &lattice, chamber, Seed(3));
        let b = anchor_cells(&interior, &lattice, chamber, Seed(3));
        assert_eq!(a, b, "same inputs, same placement");
    }

    /// The negative control on `is_faithful`. A checker that returns `true`
    /// for everything would make `every_placement_is_faithful` vacuous, and
    /// that is exactly how a green suite hides a broken embedding.
    #[test]
    fn is_faithful_rejects_a_scattered_placement() {
        let (interior, lattice, chamber) = fixture(2, Seed(5));
        let mut scattered = anchor_cells(&interior, &lattice, chamber, Seed(5));
        // Move one anchor into the fabric, where nothing can path to it.
        if let Some((_, cell)) = scattered.iter_mut().next() {
            *cell = Cell(lattice.extent.x, lattice.extent.y); // the corner wall
        }
        assert!(
            !is_faithful(&interior, &lattice, &scattered),
            "is_faithful accepted an anchor placed in the building's fabric"
        );
    }
}
```

Write a `fixture(chamber_count, seed) -> (Interior, Lattice, usize)` helper
that builds a real `Structure`, its `Lattice` via `embed_with`, and a real
`Interior` via `chamber_interior_of`. **Build the fixture from the real
derivations, not by hand** — a hand-built interior tests your model of an
interior, not an interior.

- [ ] **Step 3: Run to verify they fail**

Run: `cargo test -p hornvale-vessel --lib lattice::anchor_cells`
Expected: FAIL — the module does not exist.

- [ ] **Step 4: Implement**

Write the module. The algorithm is a **seeded placement scan**, not a solver:
walk the chamber's floor cells in row-major order, assign anchors in
`Interior::ids()` order, and accept a placement when it keeps `is_faithful`
true so far. Consume stream draws only where a genuine choice exists, and
**report the count as `dof` would** — `Lattice.dof`'s doc is the standard: the
embedder may not invent more freedom than the anchor graph leaves.

Doc comment must state: this is an **embedding**, the interior is
authoritative, and the placement may change between versions without
corrupting any world (decision 0069) — which is exactly why nothing derived
from it may be committed.

- [ ] **Step 5: Verify and commit**

```bash
cargo test -p hornvale-vessel --lib lattice::anchor_cells
cargo fmt && cargo clippy -p hornvale-vessel --all-targets -- -D warnings
git add windows/vessel/src/lattice/anchor_cells.rs windows/vessel/src/lattice/mod.rs docs/audits/type-audit-report.md
git commit -m "feat(sighting): embed the interior's anchors into lattice cells"
```

---

### Task 3: Symmetric integer shadowcasting

**Files:**
- Create: `windows/vessel/src/lattice/sight.rs`
- Modify: `windows/vessel/src/lattice/mod.rs`

**Interfaces:**
- Produces: `pub fn shadowcast(lattice: &Lattice, from: Cell, radius: i32) -> BTreeSet<Cell>`

- [ ] **Step 1: Write the failing tests, symmetry first**

```rust
#[cfg(test)]
mod tests {
    use super::*;

    /// The campaign's stated modelling commitment (spec §4). `Interior`'s
    /// adjacency is a `BTreeSet` of pairs and symmetric by construction, so
    /// an asymmetry here would be an artifact of the embedding — the thing
    /// §2.1 exists to prevent.
    ///
    /// Ordinary recursive shadowcasting FAILS this. If it passes on the
    /// first try, check that the test is actually reaching both directions
    /// before believing it.
    #[test]
    fn sight_is_symmetric() {
        let lattice = fixture(3, Seed(1));
        let floors: Vec<Cell> = lattice
            .cells
            .iter()
            .filter(|(_, k)| k.passable())
            .map(|(c, _)| *c)
            .collect();
        for &a in &floors {
            let from_a = shadowcast(&lattice, a, 12);
            for &b in &floors {
                let sees = from_a.contains(&b);
                let back = shadowcast(&lattice, b, 12).contains(&a);
                assert_eq!(sees, back, "{a:?} sees {b:?} = {sees}, but reverse = {back}");
            }
        }
    }

    #[test]
    fn a_wall_blocks_what_lies_behind_it() {
        // The negative control. Without it, a `shadowcast` that returned
        // every cell in radius would pass the symmetry test perfectly.
        let lattice = fixture(2, Seed(4));
        let (from, blocked) = a_cell_and_something_behind_a_wall(&lattice);
        assert!(
            !shadowcast(&lattice, from, 12).contains(&blocked),
            "sight passed through a wall"
        );
    }

    #[test]
    fn you_always_see_your_own_cell() {
        let lattice = fixture(1, Seed(2));
        let here = some_floor_cell(&lattice);
        assert!(shadowcast(&lattice, here, 0).contains(&here));
    }

    #[test]
    fn radius_bounds_the_result() {
        let lattice = fixture(4, Seed(9));
        let here = some_floor_cell(&lattice);
        for cell in shadowcast(&lattice, here, 3) {
            let (dx, dy) = ((cell.0 - here.0).abs(), (cell.1 - here.1).abs());
            assert!(dx <= 3 && dy <= 3, "{cell:?} is outside radius 3 of {here:?}");
        }
    }

    #[test]
    fn sight_is_deterministic() {
        let lattice = fixture(2, Seed(6));
        let here = some_floor_cell(&lattice);
        assert_eq!(shadowcast(&lattice, here, 8), shadowcast(&lattice, here, 8));
    }
}
```

- [ ] **Step 2: Run to verify they fail**

Run: `cargo test -p hornvale-vessel --lib lattice::sight`
Expected: FAIL — module missing.

- [ ] **Step 3: Implement symmetric shadowcasting**

Integer arithmetic only — no floats anywhere in this file, for the reason
`kernel/src/astar.rs` gives for its `u64` costs. A wall is a cell whose
`CellKind::passable()` is false; ask the predicate, never `== CellKind::Wall`
(`CellKind::passable`'s own doc says why: a rule written against the variant
breaks the day `Rubble` arrives).

- [ ] **Step 4: Verify, then prove the symmetry test discriminates**

Run: `cargo test -p hornvale-vessel --lib lattice::sight`
Expected: PASS.

Then **mutate**: make the algorithm asymmetric (drop the symmetric-permissive
check, or widen one octant's slope test) and confirm `sight_is_symmetric` goes
RED. Restore, confirm `git status --short` clean. **A symmetry test that
passes under an asymmetric implementation is decoration** — this is the exact
failure The Panes shipped, where a shear test could not see a shear. Paste the
RED output in your report.

- [ ] **Step 5: Commit**

```bash
cargo fmt && cargo clippy -p hornvale-vessel --all-targets -- -D warnings
git add windows/vessel/src/lattice/sight.rs windows/vessel/src/lattice/mod.rs docs/audits/type-audit-report.md
git commit -m "feat(sighting): symmetric integer shadowcasting over the lattice"
```

---

### Task 4: `marks` joins `vessel/plan/v1`

The Panes deliberately omitted this field because nothing wrote it. This
campaign is its first writer.

**Files:**
- Modify: `windows/vessel/src/plan.rs`

**Interfaces:**
- Produces:
  - `pub struct PlanMark { pub x: i32, pub y: i32, pub noun: String, pub kind: String, pub datum: String, pub salience: u32 }`
  - `SessionPlan.marks: Vec<PlanMark>` — **last** in key order, after `you`.
  - `plan_of` gains a `marks: Vec<PlanMark>` parameter, appended last.

- [ ] **Step 1: Write the failing tests**

Cover: marks round-trip; marks serialize in ascending `(salience, noun)` order
so the bytes are deterministic; an empty marks list still serializes as `[]`;
and — the property that matters — **a mark's cell is inside the extent**.

- [ ] **Step 2–4: red, implement, green**

Run `cargo test -p hornvale-vessel --lib plan::tests` between each.

The `PlanMark` shape is `scene/surrounds/v1`'s `Mark` plus a cell, and that
shape is deliberately the focalizer's `Focalized.nouns` shape — "because that
identity is what makes map and prose two grains of one lens." Say so in the
doc comment; it is why the field is not free-form.

Update `plan.rs`'s module doc: the "types here, instances elsewhere" paragraph
currently says this schema *has no marks field yet*. It does now.

- [ ] **Step 5: Commit**

```bash
cargo fmt && cargo clippy -p hornvale-vessel --all-targets -- -D warnings
git add windows/vessel/src/plan.rs docs/audits/type-audit-report.md
git commit -m "feat(sighting): vessel/plan/v1 gains marks, with its first writer"
```

---

### Task 5: Wire it into the session — creatures placed, sight narrowing

**Files:**
- Modify: `windows/vessel/src/session.rs`
- Modify: `windows/vessel/tests/session_snapshot.rs`

**Interfaces:**
- Consumes: `anchor_cells` (Task 2), `shadowcast` (Task 3), `PlanMark` (Task 4).

- [ ] **Step 1: Write the failing tests**

Four, and the fourth is the campaign's central invariant:

1. **Creatures reach the plan.** Enter a chamber holding an NPC; the snapshot's
   chamber-band plan has a mark at that NPC's cell, with the NPC's own noun.
2. **Occupancy refuses a collision.** Two creatures cannot be placed at one
   cell; `lattice::Occupancy::place`'s `Refusal` path finally has a caller, and
   this test is what makes it non-vacuous.
3. **Sight narrows what is sent.** A creature outside the possession's
   shadowcast does not appear in `sensed` or in `marks`.
4. **THE NEGATIVE CONTROL — perturbing the embedding moves what is DRAWN and
   not what is KNOWN.** Take a snapshot; perturb the anchor placement (a
   different placement seed); take another. Assert `spatial` differs and
   `known` is byte-identical. This is spec §2.1 as a test, and it is the one
   test that fails if a later change lets sight leak into belief.

- [ ] **Step 2: Run to verify they fail**

Run: `cargo test -p hornvale-vessel --test session_snapshot`

- [ ] **Step 3: Implement**

In `Session::snapshot`'s chamber branch: derive the chamber's `Interior` (the
call already exists at three sites in this file — reuse, do not add a fourth
derivation), place anchors via `anchor_cells`, resolve `liveness::Occupancy`'s
`(RoomAddr, AnchorId)` for creatures in this chamber into cells, populate
`lattice::Occupancy`, shadowcast from the possession's cell, and emit only lit
creatures as `marks`.

**Narrow `sensed.present` by the same shadowcast**, so the redaction is
structural (`CLIENT-redaction-panes`) rather than the pane's choice.

**Do not touch `self.knowledge`.** That is spec §2.1, and test 4 pins it.

- [ ] **Step 4: Green, then the full crate**

```bash
cargo test -p hornvale-vessel --test session_snapshot
cargo test -p hornvale-vessel 2>&1 | tee /tmp/hv-vessel.txt
```

Fixtures will drift — `snapshot-seed-42-chamber.json` now carries marks. That
is a **witness**, not a claim: rebaseline with `REBASELINE=1`, then **read the
diff** and confirm it is additive plus the expected marks. If any pre-existing
value moved, stop and report.

- [ ] **Step 5: Commit**

```bash
cargo fmt && cargo clippy --workspace --all-targets -- -D warnings
git add windows/vessel/src/session.rs windows/vessel/tests/ docs/audits/type-audit-report.md
git commit -m "feat(sighting): creatures stand in cells, and sight narrows what is sent"
```

---

### Task 6: Re-measure, and ratchet the cost gate

**Files:**
- Modify: `cli/tests/session_cost.rs`, `windows/vessel/examples/turn_cost.rs`,
  `clients/vessel/wasm/turn_bench.mjs`

- [ ] **Step 1: Re-run both benches** (native and ABI) and record the readings
  beside the existing ones. State the delta as a number.
- [ ] **Step 2: Update the ceilings.** They are falsification ceilings at ≈2×
  the slowest **dev-profile** reading — `scripts/gate-full-heavy.sh` runs
  `cargo nextest --profile heavy` with **no `--release`**, and a release-basis
  ceiling was nearly tripped during The Panes. `START_BUDGET_MS` keeps its
  wider multiplier; its doc comment says why.
- [ ] **Step 3:** `cargo test -p hornvale --test session_cost -- --ignored --nocapture`,
  then `cargo test -p hornvale --test heavy_tier` (the `#[ignore]` reason is
  checked for **verbatim** equality, not as a prefix).
- [ ] **Step 4: Commit.**

---

### Task 7: The client draws them

**Files:**
- Modify: `clients/vessel/src/pane_plan.ts`, `clients/vessel/src/snapshot.ts`
- Create: `clients/vessel/src/pane_plan_marks_test.ts`

- [ ] **Step 1: Extend the TS interface** — `PlanPayload.marks?: PlanMark[]`,
  **optional**, for the same reason `spatial` is: a client bundle can outlive
  the sim that produced a payload.

- [ ] **Step 2: Write the failing tests.** A mark renders its glyph at its
  cell; marks draw over the floor but **never over `@`**; a mark outside the
  extent is ignored rather than throwing; an absent `marks` renders the plan
  unchanged; a malformed mark entry is refused, not thrown on.

**Guard every dereference before using it.** The Panes shipped two Criticals of
exactly this shape in this exact file — `plan.you` and then a palette entry
three lines away — and the second survived the fix for the first. Audit every
sibling dereference in `planRows`, and say in your report what you found even
if clean.

- [ ] **Step 3–4:** red, implement, green. Glyph choice: first letter of the
  creature's `kind`, lowercased; `@` always wins its own cell.

- [ ] **Step 5:** unlit cells render as blank, not as wall — a cell you cannot
  see is unknown, not solid, and drawing it as `#` would be the map lying.

- [ ] **Step 6:** `cd clients/vessel && deno fmt --check && deno lint && deno task check && deno task test`. Do **not** run `deno task build` — Task 9 owns the bundle.

- [ ] **Step 7: Commit.**

---

### Task 8: The `scene/surrounds` version sweep

A latent defect on main, not this campaign's doing (ledger #4).

**Files:**
- Modify: `clients/vessel/src/pane_chart.ts`, `clients/vessel/src/pane_chart_test.ts`, `windows/vessel/src/snapshot.rs`

- [ ] **Step 1:** Confirm the current tag: `grep SURROUNDS_SCHEMA windows/scene/src/surrounds.rs`. It is `scene/surrounds/v2`.
- [ ] **Step 2: Write a failing test** — `chartRows` returns `null` for a chart whose `schema` is absent or unrecognised, and renders for the current tag.
- [ ] **Step 3: Implement** the check, accepting the current tag.

**Accept a known tag; do not merely reject unknown ones.** `parseSnapshot` is
the precedent — it refuses anything that is not its tag. The chart reader
survived v1→v2 only because it validated nothing, which is luck, not design.

- [ ] **Step 4:** Sweep the three stale `v1` strings: `pane_chart.ts:1`
  (comment), `pane_chart_test.ts:78` (fixture), `snapshot.rs:203` (doc).
- [ ] **Step 5:** Both gates. Commit.

---

### Task 9: Close the campaign

**This ends at the G6 hard stop.** Do not merge, do not remove the worktree.

- [ ] **Step 1:** `make gate` (foreground, `timeout: 3600000`).
- [ ] **Step 2:** `deno task build` from `clients/vessel/`, then
  `make vessel-check`. **If the byte-identity smoke fails, read which fixture
  moved before rebaselining** — this campaign moves *snapshot* bytes and must
  not move *transcript* bytes; a moved transcript means the change leaked into
  prose.
- [ ] **Step 3:** `make rebaseline`, then
  `git diff --exit-code book/src/gallery/ book/src/reference/ book/src/laboratory/ docs/audits/`.
- [ ] **Step 4: Chronicle** at `book/src/chronicle/the-sighting.md`, wired into
  `book/src/SUMMARY.md`. It must state plainly what the campaign *declined*:
  sight narrows what a client is shown, and does **not** gate what an agent
  believes.
- [ ] **Step 5: Retrospective** at `docs/retrospectives/the-sighting.md`.
  **Promote `.superpowers/sdd/` into it before teardown** — that directory is
  git-ignored and dies with the worktree.
- [ ] **Step 6: Freshness sweep + registry.** `CLIENT-tile-view` comes off
  "sight is next". Add rows for the **faithful embedding** concept and for the
  **deferred knowledge-gating**. Grep `book/src/open-questions.md` for this
  campaign's domains before concluding no Confidence Gradient bet moved —
  The Panes' close wrongly claimed N/A and the reviewer caught it.
  **No registry IDs in prose outside `book/src/frontier/`.**
- [ ] **Step 7:** Both gates again, commit, **then STOP** and present the G6
  package.

---

## Self-Review

**Spec coverage.** §2.1 the authority line → Tasks 2, 5 (test 4 is it as a
test). §2.2 faithfulness → Task 2. §3 what ships → Tasks 2–5, 7. §4 symmetry →
Task 3. §5 cost → Tasks 1, 6. §6 non-goals → nothing implements them; the
knowledge deferral is pinned by Task 5 test 4. §8 risks 1–4 → Task 2 Step 4,
Task 6, Task 9 Step 6, Task 8. §9 testing → Tasks 2, 3, 5. §10 DoD → Task 9.

**Placeholder scan.** Tasks 6 and 8–9 give steps rather than full code because
they are re-measurement, a mechanical sweep, and prose respectively. Every
task that writes new logic (2, 3, 4, 5, 7) carries its tests as code.

**Type consistency.** `anchor_cells(interior, lattice, chamber, seed) ->
BTreeMap<AnchorId, Cell>` is defined in Task 2 and called with that order in
Task 5. `shadowcast(lattice, from, radius) -> BTreeSet<Cell>` likewise.
`PlanMark`'s fields match the TS `PlanMark` in Task 7. `marks` is last in key
order in both.

**Known-defect classes this plan tries not to repeat**, all from The Panes:
- Every new guard gets a mutation proof (Tasks 3, 7), because a negative
  control that cannot see its own negative is decoration.
- Task 7 explicitly asks for a *sibling audit* of dereferences, because fixing
  one unguarded dereference gave no immunity to the next one.
- Names were checked free before use, and `Visibility` was ruled out on a
  collision — the discipline that would have caught `.casement-map`.
