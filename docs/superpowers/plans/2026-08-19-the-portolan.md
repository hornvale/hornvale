# The Portolan Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** A free-roaming cursor in the terminal client's plate: it selects a cell, the sim answers what is there, and the most specific feature's name appears in a strip beneath the map.

**Architecture:** `hornvale-game-core` owns cursor state and renders what it is handed; it depends on **no** hornvale crate and that stays true. `clients/game/bin` (which does depend on `hornvale-vessel`, in-process) answers *what is at this cell* and hands core a string. The cursor is painted by the **terminal's own hardware cursor**, not by ink in the grid.

**Tech Stack:** Rust 2024, `crossterm 0.29`. `clients/game` is **outside** the cargo workspace with its own toolchain, so the workspace dependency allowlist does not bind it — and this campaign adds no dependency regardless.

**Spec:** `docs/superpowers/specs/2026-08-19-the-portolan-design.md`

## Global Constraints

- **80×24 is the floor.** `MIN_WIDTH = 80`, `MIN_HEIGHT = 24`; `render` **refuses** anything smaller rather than degrading. "If it only works larger, it is wrong." Never weaken this.
- **Ornament may never occupy a cell that carries information.** Gutters and rules carry no ink; reserving them means *not drawing there*, never a drawn border. The cursor is the terminal's hardware cursor and occupies **no grid cell**.
- **`hornvale-game-core` depends on no hornvale crate.** It is a pure `vessel/session/v2` → character grid renderer. Do not add one.
- **No `Social` variant on `Source`, ever.** Its absence is the enforcement mechanism, not an oversight — see `cell.rs`'s doc.
- **No new dependency** in `clients/game`.
- `#![warn(missing_docs)]` — every public item, field and variant gets a one-line doc comment.
- **No schema change.** `vessel/session/v2` must not move. If a task believes it must, that is a **STOP** (cross-repo contract, additive-or-versioned only) — raise it, do not absorb it.
- **Run the suite ONCE, inspect many.** Capture to a file and grep it.
- Gate: `cargo test` in the relevant crate, plus `cargo clippy --all-targets -- -D warnings` and `cargo fmt --check`. **`make gate-commit` does NOT cover `clients/game`** — it is outside the workspace. Run the client's own checks explicitly.

---

### Task 1: Look mode

The largest thing this campaign adds, and it is not additive: `input.rs::verb_for` already binds `hjkl`, the arrows and `1`-`9` to `go <dir>`, and the client has **no mode concept at all**.

**Files:**
- Modify: `clients/game/bin/src/input.rs`
- Test: in-module `#[cfg(test)]` in `input.rs`

**Interfaces:**
- Consumes: nothing.
- Produces:
  - `pub enum Mode { Normal, Look }`
  - `pub enum Action { Verb(String), CursorBy(i16, i16), EnterLook, LeaveLook, None }`
  - `pub fn action_for(key: KeyEvent, mode: Mode) -> Action`
  - `verb_for` stays **exactly as it is** — Task 3 and the driver still call it.

- [ ] **Step 1: Read what exists before changing it**

```bash
sed -n '1,110p' clients/game/bin/src/input.rs
```

`verb_for` is a pure `KeyEvent -> Option<String>` with no state. **Keep it that way.** Build the mode *around* it, never inside it — spec F0 bounds this task's blast radius to exactly that.

- [ ] **Step 2: Write the failing tests**

```rust
    /// Normal mode is UNCHANGED. Every binding that worked before this
    /// campaign must still work identically — the mode is added around
    /// `verb_for`, never inside it, so this is checking that the wrapper is
    /// transparent rather than that the map is correct.
    #[test]
    fn normal_mode_dispatches_exactly_what_verb_for_does() {
        for code in [
            KeyCode::Up, KeyCode::Down, KeyCode::Left, KeyCode::Right,
            KeyCode::Char('h'), KeyCode::Char('j'), KeyCode::Char('k'),
            KeyCode::Char('l'), KeyCode::Char('y'), KeyCode::Char('u'),
            KeyCode::Char('b'), KeyCode::Char('n'), KeyCode::Char('.'),
            KeyCode::Char('<'), KeyCode::Char('>'), KeyCode::Char('m'),
            KeyCode::Char('?'), KeyCode::Char('1'), KeyCode::Char('5'),
            KeyCode::Char('9'),
        ] {
            let key = KeyEvent::new(code, KeyModifiers::NONE);
            match (verb_for(key), action_for(key, Mode::Normal)) {
                (Some(v), Action::Verb(a)) => assert_eq!(v, a, "{code:?} changed meaning"),
                (None, Action::None) => {}
                (v, a) => panic!("{code:?}: verb_for gave {v:?} but action_for gave {a:?}"),
            }
        }
    }

    /// `x` enters look mode. Chosen because it is FREE — the taken set is
    /// `? . < > 1-9 b h j k l m n Q u y`, checked, and `x` is the roguelike
    /// convention for exactly this.
    #[test]
    fn x_enters_look_mode_and_escape_leaves_it() {
        let x = KeyEvent::new(KeyCode::Char('x'), KeyModifiers::NONE);
        assert!(matches!(action_for(x, Mode::Normal), Action::EnterLook));
        let esc = KeyEvent::new(KeyCode::Esc, KeyModifiers::NONE);
        assert!(matches!(action_for(esc, Mode::Look), Action::LeaveLook));
    }

    /// In look mode the SAME movement keys drive the cursor instead of the
    /// character. This is the collision the mode exists to resolve, so it is
    /// asserted for every direction rather than sampled.
    #[test]
    fn look_mode_moves_the_cursor_not_the_character() {
        for (code, dx, dy) in [
            (KeyCode::Char('h'), -1i16, 0i16), (KeyCode::Char('l'), 1, 0),
            (KeyCode::Char('k'), 0, -1),       (KeyCode::Char('j'), 0, 1),
            (KeyCode::Char('y'), -1, -1),      (KeyCode::Char('u'), 1, -1),
            (KeyCode::Char('b'), -1, 1),       (KeyCode::Char('n'), 1, 1),
            (KeyCode::Left, -1, 0),            (KeyCode::Right, 1, 0),
            (KeyCode::Up, 0, -1),              (KeyCode::Down, 0, 1),
        ] {
            let key = KeyEvent::new(code, KeyModifiers::NONE);
            match action_for(key, Mode::Look) {
                Action::CursorBy(gx, gy) => assert_eq!((gx, gy), (dx, dy), "{code:?}"),
                other => panic!("{code:?} in look mode gave {other:?}, wanted CursorBy"),
            }
            // The same key in normal mode must still move the CHARACTER.
            assert!(
                matches!(action_for(key, Mode::Normal), Action::Verb(_)),
                "{code:?} lost its normal-mode meaning"
            );
        }
    }

    /// A key with no meaning in look mode does NOT fall through to its
    /// normal-mode verb — that would walk the character while the player
    /// believes they are looking.
    #[test]
    fn look_mode_does_not_fall_through_to_movement_verbs() {
        let enter_room = KeyEvent::new(KeyCode::Char('>'), KeyModifiers::NONE);
        assert!(
            !matches!(action_for(enter_room, Mode::Look), Action::Verb(_)),
            "look mode leaked a verb"
        );
    }
```

- [ ] **Step 3: Run to verify they fail**

```bash
cd clients/game && cargo test -p hornvale-game --lib input:: > /tmp/hv-p1.log 2>&1; echo "exit=$?"
grep -E "^test result|cannot find|error\[" /tmp/hv-p1.log | head
```

Expected: compile error — `Mode`, `Action`, `action_for` are not defined.

- [ ] **Step 4: Implement**

Add the two enums and `action_for`. `action_for(key, Mode::Normal)` delegates to `verb_for` and wraps its `Option<String>` into `Action::Verb`/`Action::None`, except for `x` which becomes `EnterLook`. `action_for(key, Mode::Look)` maps the twelve movement keys to `CursorBy`, `Esc` to `LeaveLook`, and **everything else to `Action::None`** — the fall-through test pins that.

- [ ] **Step 5: Run to verify they pass**

```bash
cd clients/game && cargo test -p hornvale-game --lib input:: > /tmp/hv-p1.log 2>&1; echo "exit=$?"
grep -E "^test result" /tmp/hv-p1.log
```

Expected: 4 passed.

- [ ] **Step 6: Commit**

```bash
cd clients/game && cargo fmt && cargo clippy --all-targets -- -D warnings
cd ../.. && git commit -m "feat(game): look mode, added around verb_for rather than inside it" -- clients/game/bin/src/input.rs
```

---

### Task 2: The cursor, and the strip

**Files:**
- Modify: `clients/game/core/src/spread.rs`
- Modify: `clients/game/core/src/lib.rs` (the `render` entry point)
- Create: `clients/game/core/src/strip.rs`
- Modify: `clients/game/bin/src/term.rs`
- Test: in-module `#[cfg(test)]` in `strip.rs`, plus `clients/game/core/tests/` as needed

**Interfaces:**
- Consumes: **nothing from Task 1.** `Mode`/`Action` live in
  `clients/game/bin/src/input.rs`, and `hornvale-game-core` has **no
  dependency on bin** — do not try to import them into core. This task's
  `term.rs` half needs only a cursor position, not a mode. Task 1 and Task 2
  are independent.
- Produces:
  - `pub struct Cursor { pub x: u16, pub y: u16 }`
  - `pub fn render_with(json: &str, w: u16, h: u16, cursor: Option<Cursor>, strip: Option<&str>) -> Result<(Grid, Option<(u16, u16)>), Error>` — the grid, plus the **screen** position the terminal cursor should sit at.
  - `strip::draw(text: &str, into: &mut Grid, origin: (u16, u16), width: u16)`
  - `render` (the existing signature) stays, delegating to `render_with(json, w, h, None, None)` — **do not break it**, `tests/` and any caller depend on it.

- [ ] **Step 1: Establish the floor BEFORE changing the layout**

The strip costs a row the plate has today, and 80×24 is a hard floor. Record what the plate height is now, so Step 5 can show what it became:

```bash
cd clients/game && cargo test -p hornvale-game-core --lib spread:: > /tmp/hv-p2.log 2>&1; echo "exit=$?"
grep -E "^test result" /tmp/hv-p2.log
sed -n '1,40p' core/src/spread.rs
```

- [ ] **Step 2: Write the failing tests**

```rust
    /// H3: the floor holds. At exactly 80x24, with the strip added, the
    /// spread still renders — and 79x24 is still REFUSED rather than
    /// degraded. This is the constraint the campaign inherits and may not
    /// weaken, so it is asserted in both directions.
    #[test]
    fn the_eighty_by_twentyfour_floor_survives_the_strip() {
        let json = fixture_json();
        let (grid, _) = render_with(&json, 80, 24, None, Some("Vngashngatva")).expect("renders at the floor");
        assert_eq!(grid.width(), 80);
        assert_eq!(grid.height(), 24);
        assert!(matches!(
            render_with(&json, 79, 24, None, None),
            Err(crate::Error::TooSmall { .. })
        ), "79 columns must still be refused, not degraded");
    }

    /// The strip is PLATE width, not full width — the endpaper keeps its own
    /// full-width row and its own job (an identity strip).
    #[test]
    fn the_strip_never_writes_past_the_plate() {
        let mut grid = Grid::new(80, 24);
        strip::draw(&"z".repeat(200), &mut grid, (0, 20), crate::spread::PLATE_WIDTH);
        for x in crate::spread::PLATE_WIDTH..80 {
            assert!(grid.get(x, 20).glyph.is_none(), "strip wrote into the entry at x={x}");
        }
    }

    /// The cursor is NOT ink. It must occupy no grid cell — `render_with`
    /// reports a position for the terminal to place its own cursor at, and
    /// the grid is byte-identical with and without one.
    #[test]
    fn the_cursor_occupies_no_cell() {
        let json = fixture_json();
        let (plain, none_at) = render_with(&json, 80, 24, None, None).expect("renders");
        let (with, some_at) =
            render_with(&json, 80, 24, Some(Cursor { x: 3, y: 4 }), None).expect("renders");
        assert!(none_at.is_none());
        assert_eq!(some_at, Some((3, 4)), "the cursor position is reported, not drawn");
        for y in 0..24 {
            for x in 0..80 {
                assert_eq!(plain.get(x, y), with.get(x, y), "cursor inked cell ({x},{y})");
            }
        }
    }

    /// A strip longer than the plate is TRUNCATED, never wrapped — wrapping
    /// would steal a row from the plate silently.
    #[test]
    fn an_overlong_strip_truncates_rather_than_wrapping() {
        let mut grid = Grid::new(80, 24);
        strip::draw(&"z".repeat(200), &mut grid, (0, 20), crate::spread::PLATE_WIDTH);
        assert!(grid.get(0, 21).glyph.is_none(), "the strip wrapped into the next row");
    }
```

`fixture_json()` does not exist — `clients/game/core/tests/` already reads committed `vessel/session/v2` fixtures (`clients/game/core/tests/fixtures/`). Reuse whichever the existing spread tests use; **do not mint a new fixture.**

`Grid::get` may not exist — check `cell.rs`. If it does not, add it (`pub fn get(&self, x: u16, y: u16) -> Cell`) with a doc comment, since three of these tests need it.

- [ ] **Step 3: Run to verify they fail**

```bash
cd clients/game && cargo test -p hornvale-game-core --lib > /tmp/hv-p2.log 2>&1; echo "exit=$?"
grep -E "^test result|cannot find|error\[" /tmp/hv-p2.log | head
```

- [ ] **Step 4: Implement**

`strip.rs` draws one row, clipped to `width`, attributed `Source::Chrome` **only if the text is chrome** — a resolved feature name is world-derived, so attribute it to whatever channel the resolution came from, and say which in the doc comment. Re-read `cell.rs`'s `Source` doc first: every drawn cell must name a channel, and there are exactly two categories.

`spread::compose` gains the strip row beneath the plate; `render_with` threads the cursor through and returns its screen position without drawing it.

- [ ] **Step 5: Run, and report what the floor became**

```bash
cd clients/game && cargo test -p hornvale-game-core > /tmp/hv-p2.log 2>&1; echo "exit=$?"
grep -E "^test result" /tmp/hv-p2.log
```

**Report the plate's content height at 80×24 before and after.** Spec F1 is settled by that number, not by the tests passing.

- [ ] **Step 6: Show the terminal cursor**

In `term.rs`: at setup, replace `Hide` with `Show` **and** `SetCursorStyle(SetCursorStyle::BlinkingUnderScore)` (crossterm 0.29, `cursor.rs:374`). After painting, `MoveTo` the reported position when there is one; when there is none, keep the cursor parked out of the way (its old hidden behaviour is fine — `Hide` when `None`).

- [ ] **Step 7: Commit**

```bash
cd clients/game && cargo fmt && cargo clippy --all-targets -- -D warnings
cd ../.. && git commit -m "feat(game): a cursor that is not ink, and a plate-width strip" -- clients/game/
```

---

### Task 3: Resolution

**Files:**
- Create: `windows/worldgen/src/resolve.rs`
- Modify: `windows/worldgen/src/lib.rs`
- Modify: `domains/terrain/src/landscape.rs` (declared salience)
- Modify: `clients/game/bin/src/driver.rs`
- Test: in-module `#[cfg(test)]`

**Interfaces:**
- Consumes: `FeatureClass`, `FeatureId`, `Feature`, `FeatureIndex` (`domains/terrain`); `gazetteer_features`, `feature_name` (`windows/worldgen`); `Cursor`, `render_with` (Task 2).
- Produces:
  - `FeatureClass::salience(self) -> u8` — **declared**, lower is more specific.
  - `pub struct CellFeatureIndex` with `pub fn build(features: &[Feature]) -> CellFeatureIndex` and `pub fn at(&self, cell: CellId) -> &[FeatureId]`.
  - `pub fn resolve_at(...) -> Option<String>` in `windows/worldgen` — the most specific feature's name.

- [ ] **Step 1: Write the failing tests**

```rust
    /// Salience is DECLARED per class, not inferred from extent. Lower is
    /// more specific. The cursor names the volcano, not the continent it
    /// stands on — that is the whole ordering, and inferring it from size
    /// would break the moment a class arrives whose size does not track its
    /// specificity.
    #[test]
    fn salience_is_declared_most_specific_first() {
        use FeatureClass::*;
        assert!(Volcano.salience() < Landmass.salience());
        assert!(SaltLake.salience() < Landmass.salience());
        assert!(River.salience() < Landmass.salience());
        assert!(Landmass.salience() < Sea.salience() || Sea.salience() < Landmass.salience(),
                "every pair must be ordered; a tie makes the cursor unpredictable");
        let mut ranks: Vec<u8> = [Volcano, Landmass, Sea, SaltLake, River]
            .iter().map(|c| c.salience()).collect();
        ranks.sort_unstable();
        ranks.dedup();
        assert_eq!(ranks.len(), 5, "salience must be a total order — no two classes may tie");
    }

    /// The index answers containment, and it agrees with the extents it was
    /// built from. Checked against EVERY cell of every feature rather than
    /// sampled, because a partial index reads exactly like a complete one.
    #[test]
    fn the_index_agrees_with_the_extents_it_was_built_from() {
        let feats = test_features();
        let index = CellFeatureIndex::build(&feats);
        for f in &feats {
            for cell in &f.extent {
                assert!(index.at(*cell).contains(&f.id), "index lost {:?} at {cell:?}", f.id);
            }
        }
        for f in &feats {
            for id in index.at(*f.extent.iter().next().expect("nonempty")) {
                let owner = feats.iter().find(|g| g.id == *id).expect("index invented a feature");
                assert!(owner.extent.contains(f.extent.iter().next().expect("nonempty")));
            }
        }
    }

    /// A cell with no feature resolves to None, not to an empty string and
    /// not to a panic. 377 of seed 42's 40,962 cells are like this.
    #[test]
    fn a_cell_with_no_feature_resolves_to_none() {
        let index = CellFeatureIndex::build(&[]);
        assert!(index.at(CellId(0)).is_empty());
    }
```

- [ ] **Step 2: Run to verify they fail**

```bash
cargo test -p hornvale-worldgen --lib resolve:: > /tmp/hv-p3.log 2>&1; echo "exit=$?"
grep -E "^test result|cannot find" /tmp/hv-p3.log | head
```

- [ ] **Step 3: Implement salience and the index**

`salience()` is a `match` returning a distinct `u8` per class, with a doc comment stating: lower is more specific; it mirrors the scene protocol's sense (`Mark.salience`, agent 5 / flagship 10 / other 20); **it is declared, not derived from extent**; and a new class must state where it sits.

`CellFeatureIndex` is a `BTreeMap<CellId, Vec<FeatureId>>` built in one pass, each cell's vec sorted by `salience()` then `FeatureId`. **Built once at world load — the feature stack is immutable for the world's lifetime, so this is never invalidated.**

- [ ] **Step 4: Wire the driver**

`driver.rs` holds `Mode`, a `Cursor`, and the index. On `Action::CursorBy`, move the cursor (clamped to the plate), resolve, and pass the name to `render_with` as the strip text. On `EnterLook`/`LeaveLook`, toggle. On `Action::Verb`, behave exactly as today.

- [ ] **Step 4b: The cursor exists at every band, and refuses honestly**

Spec §2: the cursor is a **plate primitive**, not a map feature — the plate
already dispatches by band ("the walk-band chart outdoors, the chamber-band
floor plan indoors"). This campaign implements the **world** row only, but the
cursor must exist at all of them.

So: entering look mode indoors, or anywhere the plate is not the world map,
must put a cursor on screen and resolve to **"nothing here yet"** — not panic,
not silently do nothing, and not resolve against the world index for a plate
that is not showing the world.

```rust
    /// The cursor is a plate primitive. At a band whose resolver does not
    /// exist yet, it still moves and still reports — it refuses honestly
    /// rather than resolving against the wrong plate. Faking a resolution
    /// here would be worse than refusing, because a wrong name is
    /// indistinguishable from a right one.
    #[test]
    fn look_mode_at_an_unresolved_band_refuses_rather_than_resolving() {
        let session = chamber_band_session();
        let mut driver = Driver::for_test(session);
        driver.apply(Action::EnterLook);
        driver.apply(Action::CursorBy(1, 0));
        assert!(driver.cursor().is_some(), "the cursor must exist at every band");
        assert_eq!(driver.strip_text(), Some("nothing here yet"));
    }
```

`chamber_band_session()` and `Driver::for_test` may not exist —
`clients/game/bin/tests/driver.rs` already drives a session in tests; reuse
its idiom rather than inventing a second. If the driver has no seam for this,
say so and report what the smallest seam would be **instead of widening the
driver's public surface to suit one test**.

- [ ] **Step 5: Measure F2, F3 and H1**

Report all three; they are the task's real deliverable:

- **F2** — resolution cost per cursor move. **Decision rule:** above 1 ms, cache the drawn name rather than re-deriving per keypress; below, do nothing.
- **F3** — the index's size for seed 42, **and the extent total it was built from**. This number is what a later campaign needs to decide whether `Feature::extent` must change shape. Do not act on it; report it.
- **H1** — the fraction of resolvable cells whose most-specific name fits `PLATE_WIDTH`. **Preregistered: at least 95%.** If it is lower, H1 is falsified and that is a shippable finding — report it, do **not** widen the plate or abbreviate to rescue it.

- [ ] **Step 6: Confirm F4 explicitly**

State plainly whether `vessel/session/v2` changed. It should not have. **If it did, STOP** — that is a cross-repo contract change and must be raised, not absorbed.

- [ ] **Step 7: Commit**

```bash
cargo fmt && make gate-commit
cd clients/game && cargo fmt && cargo clippy --all-targets -- -D warnings
cd ../.. && git commit -m "feat: declared salience, the cell index, and cursor resolution" -- domains/terrain/ windows/worldgen/ clients/game/
```

---

### Task 4: H2 — the cursor is predictable

**Files:**
- Create: `windows/worldgen/tests/portolan_resolution.rs`

**Interfaces:**
- Consumes: `CellFeatureIndex`, `resolve_at`, `FeatureClass::salience` (Task 3).

- [ ] **Step 1: Write the measurement**

**H2:** the same cell resolves to the same name on repeat visits within a session, and across a rebuild of the world from its seed. *Falsified by any variation*, which would mean the ordering is not total and Task 3's salience has a gap.

Build seed 42 twice from scratch, resolve every cell in both, and assert the answers are identical.

- [ ] **Step 2: Run and record**

```bash
cargo test -p hornvale-worldgen --test portolan_resolution -- --nocapture > /tmp/hv-h2.log 2>&1; echo "exit=$?"
cat /tmp/hv-h2.log
```

- [ ] **Step 3: Commit**

```bash
cargo fmt && make gate-commit
git commit -m "test(worldgen): H2 — cursor resolution is stable across rebuilds" -- windows/worldgen/tests/
```

---

### Task 5: Close

- [ ] **Step 1: Delete both spike instruments**

```bash
git rm windows/worldgen/examples/portolan_spike.rs windows/worldgen/examples/portolan_resolution_spike.rs
```

- [ ] **Step 2: Chronicle, retrospective, registry rows, decision**

- `book/src/chronicle/the-portolan.md` + `book/src/SUMMARY.md`.
- `docs/retrospectives/the-portolan.md` + its line in `docs/retrospectives/README.md`. **Write this LAST, after the final review** — The Gazetteer's retrospective was authored at its close task and under-reported itself by three findings, which cost a second merge to repair.
- The chronicle must carry: **the first spike was refuted by the second**, and why (a measurement inherits its question's framing); the nesting result (99.84%); that **zoom does not separate a stack**; and that **a volcano is not always on land** (`Volcano ⊂ Sea`, 6.9%).
- Decision record for spec §11, numbered **contiguously from main at the time of writing**. Check `make board` first; `docs_consistency` asserts no two records share a leading number.
- Registry rows for spec §9's carried-forward items.

- [ ] **Step 3: Freshness sweep**

The book may never lag merged reality (decision 0030 if a Confidence Gradient bet moved).

- [ ] **Step 4: Absorb main, then regenerate**

**Do this at every stage boundary, not only here.** The Gazetteer ran ten tasks 71 commits behind and found `docs/audits/type-audit-report.md` had auto-merged **cleanly and wrong**:

```bash
git merge origin/main
make rebaseline
git diff --exit-code -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$')
```

Never infer freshness from a clean merge.

- [ ] **Step 5: Stage, then merge**

```bash
git push -u origin campaign/the-portolan
make sluice-stage BRANCH=campaign/the-portolan REF=$(git rev-parse HEAD)
```

The merge is a **hard stop for Nathan**. It needs a `Sluice-Headline:` trailer sharing the `Claude-Session` trailer block with **no blank line between them**.
