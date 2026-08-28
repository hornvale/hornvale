# The Overture Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make world generation's 3.05 s (projected: a minute) into the first
part of the game rather than the delay before it, and make it not recur.

**Architecture:** A **frame with pluggable views**. The frame owns the chrome,
the progress substrate and a `space` cycle; a view owns the middle and must
render honestly at any build rung. Four views arrive in the order the world can
justify them — `sky` (data complete at 0.4 ms), `atlas` (202 ms), `tongue`
(2,043 ms), `almanac` (grows throughout). Plus a world cache whose validity is
seed + pins + an existing label-diff + a 0.4 ms prefix tripwire.

**Tech Stack:** Rust 2024, `clients/game` (own workspace, outside the cargo
workspace — crossterm/signal-hook allowed, decision 0055), one additive callback
in `windows/worldgen`. `cargo nextest`.

**Spec:** `docs/superpowers/specs/2026-08-28-the-overture-design.md`

## Global Constraints

- **No new workspace dependency.** `serde`, `serde_json`, `libm` only
  (decision 0004). `clients/` is exempt and adds nothing new — **`hornvale-language`
  is ALREADY a direct dependency** at `clients/game/bin/Cargo.toml:32`.
- **No `HashMap`/`HashSet`** — `BTreeMap`/`BTreeSet`/`Vec` only, enforced by
  `clippy.toml` `disallowed-types`, which reaches `clients/`.
- **Every crate sets `#![warn(missing_docs)]`**; `type-audit:` tags on the ITEM's
  doc comment, never a field's. `clients/` is outside type-audit's scanned roots;
  `windows/worldgen` is NOT, so Task 2's callback needs real tags.
- **`cargo fmt --all` last, then `make gate-commit`,** before every commit.
- Use **`make game-check`** (timed, leaves a ledger row), never `game-check-run`.
- **Run the suite ONCE and inspect many** — capture to a file, then grep it.
- **No committed byte may move.** Nothing here is in the determinism path.
- **`make rebaseline` HAZARD:** it OOM-killed during the previous campaign and
  **truncated a committed artifact to empty BEFORE reporting its failure** —
  every redirect-written artifact is truncated at `open()` time. Check `uptime`
  first; run `git status` immediately after; a failed rebaseline is not a no-op
  you can retry from.
- **Do not delete a test to make the build pass.** If a test's subject moved,
  retarget it and say so.

## What this campaign keeps re-learning — read before Task 1

The previous campaign caught **eleven claims that could not discriminate**:
eight tests whose input space collapsed to a single value, two suites that would
**hang** rather than fail, and one comment asserting coverage a test did not
provide. Three brief items were measured **unbuildable** rather than approximated.

So, per task: if an assertion could pass against a gutted implementation, guard
it explicitly and say in your report how you know it discriminates. Mutation-prove
on the **whole test binary, unfiltered**, and report the exit code — a
name-filtered mutation run misled that campaign once and made its own evidence
look weaker than the truth.

---

### Task 1: Open the terminal before genesis

**Files:**
- Modify: `clients/game/bin/src/main.rs` (`run`, ~:45–62)
- Test: `clients/game/bin/tests/` (new file or the existing suite)

**Interfaces:**
- Consumes: `term::Term::{open, draw, draw_text, restore}` (`term.rs:62,109,159`).
- Produces: a `run` in which `Term::open()` precedes `Driver::start`, and a
  documented error path that restores the terminal before printing.

- [ ] **Step 1: Write the failing test**

```rust
#[test]
fn a_failing_genesis_still_leaves_the_terminal_restored() {
    // The reason `Term::open()` currently sits AFTER `Driver::start`: a genesis
    // failure prints to a clean shell rather than needing raw mode torn down
    // first. Inverting that is the precondition for every view, so the campaign
    // owes the cost back as a test rather than as a hope.
    //
    // Drives the seam, not the real terminal: `run` is refactored so the
    // start-and-report path is a pure function of a `TermHandle`, and this test
    // passes a recording double that asserts `restore` was called before the
    // error text was produced.
    let term = RecordingTerm::default();
    let err = start_and_report(&term, /* a seed whose genesis fails */ bad_seed());
    assert!(err.is_err(), "the test's premise: this seed must fail genesis");
    assert!(
        term.restored_before_reporting(),
        "the terminal was not restored before the error was printed"
    );
}
```

`RecordingTerm`, `start_and_report` and `bad_seed` do not exist — you write them.
**Adapt to the real API rather than inventing one to match the sketch.** If no
seed fails genesis, inject the failure at the seam instead and say so.

- [ ] **Step 2: Run to verify it fails.** Expected: FAIL, the seam does not exist.

- [ ] **Step 3: Implement.** `Term::open()` moves ahead of `Driver::start`; the
  error path restores before printing. Keep `Drop for Term`'s restore as the
  backstop it already is — this adds an explicit path, it does not replace it.

- [ ] **Step 4: Run to verify it passes.**

- [ ] **Step 5: `cargo fmt --all`, `make gate-commit`, commit.**

---

### Task 2: The observer callback on `build_to`'s existing rung boundaries

**Files:**
- Modify: `windows/worldgen/src/lib.rs` — `build_to` (~:7495) and its three
  boundaries at **:7880** (`depth == BuildDepth::Astronomy`), **:7913**
  (`depth <= BuildDepth::Terrain`), **:8422** (`depth <= BuildDepth::Settlements`)
- Test: `windows/worldgen/tests/suite.rs`

**Interfaces:**
- Produces: an observer parameter — shape is yours, but it must let a caller
  learn *which rung just completed* and read the partially-built `World`.
- **Every existing entry point keeps its current signature.** `build_world`,
  `build_world_to`, `build_world_from_components` must not change for callers who
  do not want an observer; add the observing variant beside them.

- [ ] **Step 1: Write the failing test**

```rust
#[test]
fn the_observer_fires_once_per_rung_in_ladder_order() {
    // The rungs are a byte-identical prefix chain, so an observer at each
    // boundary sees a REAL world at that depth, not a half-built one. That is
    // what lets a view render honestly at any rung.
    let mut seen: Vec<BuildDepth> = Vec::new();
    let _ = build_world_observed(
        Seed(42), &SkyPins::default(), SkyChoice::Generated,
        &TerrainPins::default(), &SettlementPins::default(), &wc,
        BuildDepth::Full,
        &mut |rung, world| {
            seen.push(rung);
            // NON-VACUITY: an observer that fired with an empty world would
            // satisfy a mere count. Assert the world GROWS across rungs.
            assert!(world.ledger.len() > 0, "rung {rung:?} handed an empty world");
        },
    ).expect("seed 42 builds");
    assert_eq!(
        seen,
        vec![BuildDepth::Astronomy, BuildDepth::Terrain,
             BuildDepth::Settlements, BuildDepth::Full],
        "rungs must fire once each, in ladder order"
    );
}

#[test]
fn an_observed_build_is_byte_identical_to_an_unobserved_one() {
    // The load-bearing one. An observer must be a READ. If observing changes a
    // byte, it has entered the determinism path and the whole design is unsafe.
    let plain = build_world(Seed(42), /* … */).expect("builds");
    let observed = build_world_observed(Seed(42), /* … */, &mut |_, _| {}).expect("builds");
    assert_eq!(plain.to_json(), observed.to_json(),
        "observing changed the world's bytes");
}
```

`world.ledger.len()` may not be the right accessor — check and use the real one.

- [ ] **Step 2: Run to verify both fail.** Expected: FAIL, `build_world_observed`
  does not exist.

- [ ] **Step 3: Implement.** The three boundaries already exist as early returns;
  the `Full` case falls through to the end. Fire the callback at each.
  **`type-audit:` tags are required here** — `windows/` IS scanned.

- [ ] **Step 4: Run to verify they pass**, then run the whole worldgen suite
  once and grep it. **Mutation-prove:** drop one callback site and confirm the
  order test dies.

- [ ] **Step 5: `cargo fmt --all`, `make gate-commit`, commit.**

---

### Task 3: The frame

**Files:**
- Create: `clients/game/bin/src/overture/mod.rs` (the frame), `.../view.rs` (the contract)
- Modify: `clients/game/bin/src/lib.rs`, `main.rs`
- Test: in-module `#[cfg(test)]`

**Interfaces:**
- Consumes: Task 2's observer; `hornvale_game_core::Grid`.
- Produces: **the `View` trait — every later task implements it, so its methods
  are named here and nowhere else:**

```rust
pub trait View {
    /// The name shown in the frame's header and used by the cycle.
    fn name(&self) -> &'static str;
    /// Whether this view has anything HONEST to say at `rung`. Contract rule 2:
    /// a view that cannot speak is SKIPPED by the frame, never rendered blank.
    fn can_speak(&self, rung: BuildDepth) -> bool;
    /// Render into a `w` x `h` region. Only called when `can_speak` is true.
    fn render(&self, world: &World, w: u16, h: u16) -> Grid;
}
```

  plus `Frame::{new, observe, cycle, current_name, visible_views}` and a
  `progress_line(&BuildState) -> String`. Tasks 4–7 each implement `View`; no
  later task introduces a method that is not on it.

**The contract, from spec §3 — a view is handed the build state, the partial
world and a pacing hint, and must:**
1. render honestly at ANY rung, including the first;
2. show what EXISTS, never a placeholder for what does not yet;
3. fill the time it is given without implying a total it cannot know.

- [ ] **Step 1: Write the failing tests**

```rust
#[test]
fn the_progress_substrate_names_phases_and_never_a_global_percentage() {
    // Spec §1: one phase is 60% of the whole, so a global bar would spend most
    // of its life inside one item. This is a design constraint the measurements
    // forced, not a style preference — pin it so nobody "improves" it later.
    let line = progress_line(&BuildState::at(BuildDepth::Settlements, 0.4));
    assert!(line.contains("the peoples"), "the phase must be named");
    assert!(!line.contains('%'), "a global percentage is forbidden: {line}");
}

#[test]
fn a_view_that_cannot_speak_at_a_rung_is_skipped_not_blanked() {
    // Contract rule 2. A view with nothing honest to say must DECLARE that,
    // so the frame can skip it — rather than rendering an empty region that
    // reads as a hang.
    let mut frame = Frame::new(vec![Box::new(SilentUntilTerrain::default())]);
    frame.observe(BuildDepth::Astronomy, &astronomy_only_world());
    assert!(frame.visible_views().is_empty(),
        "a view with nothing to say must be skipped, not shown blank");
    frame.observe(BuildDepth::Terrain, &terrain_world());
    assert_eq!(frame.visible_views().len(), 1);
}

#[test]
fn space_cycles_only_among_views_that_can_speak() {
    // Non-vacuity guard: with one view registered this passes trivially, so
    // register THREE and assert the cycle visits exactly the speaking ones.
    let mut frame = Frame::new(vec![
        Box::new(AlwaysSpeaks::named("a")),
        Box::new(SilentUntilTerrain::default()),
        Box::new(AlwaysSpeaks::named("c")),
    ]);
    frame.observe(BuildDepth::Astronomy, &astronomy_only_world());
    let mut visited = Vec::new();
    for _ in 0..4 { visited.push(frame.current_name().to_string()); frame.cycle(); }
    assert_eq!(visited, vec!["a", "c", "a", "c"], "cycle visited a silent view");
}
```

- [ ] **Step 2: Run to verify they fail.**

- [ ] **Step 3: Implement** the trait, the frame, the progress substrate and the
  cycle. The per-phase bar reads the previous run's timings **from disk**; a
  first-ever run shows names and a fact count and **no bar**, which is the honest
  state (spec §2).

- [ ] **Step 4: Run to verify they pass.**

- [ ] **Step 5: `cargo fmt --all`, `make gate-commit`, commit.**

---

### Task 4: View — `sky`

**Files:**
- Create: `clients/game/bin/src/overture/sky.rs`
- Test: in-module

**Interfaces:**
- Consumes: `hornvale_astronomy::neighborhood::Neighbor` — carries `class`,
  `distance`, `apparent_brightness`, `color`, **`declination`**,
  **`right_ascension`** (`domains/astronomy/src/neighborhood.rs:14`);
  `hornvale_almanac::NightSkyLines` — `pole_star`, `heliacal`, `wanderers`,
  `figures`, `eclipses`, `alignment` (`windows/almanac/src/lib.rs:104`).

**Why this view is first: its data is complete at 0.4 ms.** It opens *complete*
where every other view opens empty.

- [ ] **Step 1: Write the failing test**

```rust
#[test]
fn the_sky_is_complete_at_the_very_first_rung() {
    // The property that earns this view its place in the ladder: astronomy
    // finishes before anything else exists, so `sky` must render fully at
    // BuildDepth::Astronomy and not improve at later rungs.
    let early = sky_view().render(&astronomy_only_world(), 78, 20);
    let late  = sky_view().render(&full_world(), 78, 20);
    assert_eq!(early.to_plain_text(), late.to_plain_text(),
        "the sky changed after astronomy; it must be complete at rung 0");
    // NON-VACUITY: two blank grids would also be equal.
    assert!(early.to_plain_text().chars().any(|c| c == '*' || c == '·'),
        "the sky drew no stars at all");
}

#[test]
fn a_star_lands_where_its_own_coordinates_put_it() {
    // Placement must come from the star's OWN right ascension and declination,
    // not from an index or an arbitrary scatter — the same by-construction
    // discipline The Quadrat's perception overlay used.
    let n = Neighbor { right_ascension: 90.0, declination: 45.0, /* … */ };
    let (col, row) = sky_position(&n, 78, 20);
    assert_eq!((col, row), expected_from(90.0, 45.0, 78, 20));
}
```

- [ ] **Step 2–5:** fail, implement, pass, gate, commit. **Mutation-prove** the
  placement: perturb `right_ascension` in the projection and confirm a test dies.

---

### Task 5: View — `atlas`

**Files:**
- Create: `clients/game/bin/src/overture/atlas.rs`
- Test: in-module

**Interfaces:**
- Consumes: `plate::draw_terrain_layer` / `draw_feature_layer` and `TileCache`
  from The Quadrat — **the renderer already exists; do not write a second one.**

- [ ] **Step 1: Write the failing test**

```rust
#[test]
fn the_atlas_is_silent_before_terrain_and_speaks_after() {
    // Contract rule 2, on the view where it matters most: at rung 0 there is no
    // terrain, so the atlas must DECLARE it cannot speak rather than drawing an
    // empty plate that reads as a broken map.
    assert!(!atlas_view().can_speak(BuildDepth::Astronomy));
    assert!(atlas_view().can_speak(BuildDepth::Terrain));
}

#[test]
fn settlements_appear_in_the_atlas_only_once_placed() {
    // The prefix property, visible: a settlement glyph before the settlements
    // rung would be a placeholder for something that does not exist.
    let at_terrain = atlas_view().render(&terrain_world(), 78, 20);
    let at_full    = atlas_view().render(&full_world(), 78, 20);
    assert!(!at_terrain.to_plain_text().contains(plate::SETTLEMENT_GLYPH));
    assert!(at_full.to_plain_text().contains(plate::SETTLEMENT_GLYPH),
        "settlements never appeared even at Full");
}
```

- [ ] **Step 2–5:** fail, implement, pass, gate, commit.

---

### Task 6: View — `almanac`, and the component registry

**Files:**
- Create: `clients/game/bin/src/overture/almanac.rs`, `.../component.rs`
- Test: in-module

**Interfaces:**
- Produces: a component registry — each component declares **the rung it needs**
  and is skipped until that rung lands. This is spec §3's component level, and it
  is what lets the almanac grow without a redesign.

- [ ] **Step 1: Write the failing tests**

```rust
#[test]
fn a_component_is_skipped_until_its_rung_lands() {
    let reg = ComponentRegistry::new(vec![
        Box::new(OrbitParagraph),      // needs Astronomy
        Box::new(OceansParagraph),     // needs Terrain
        Box::new(PeoplesParagraph),    // needs Settlements
    ]);
    let early = reg.render(BuildDepth::Astronomy, &astronomy_only_world());
    assert_eq!(early.len(), 1, "only the orbit paragraph can speak at rung 0");
    let late = reg.render(BuildDepth::Full, &full_world());
    assert_eq!(late.len(), 3);
}

#[test]
fn what_is_strange_names_only_what_is_true_of_this_world() {
    // Nathan's own component: "no oceans" / "tidally locked" / "a wasteland".
    // NON-VACUITY: seed 42 HAS oceans and is NOT locked, so a component that
    // always fired would fail here — assert both directions on one world.
    let lines = strange_lines(&full_world());
    assert!(!lines.iter().any(|l| l.contains("no oceans")),
        "seed 42 has oceans; the component claimed otherwise");
    assert!(!lines.iter().any(|l| l.contains("tidally locked")),
        "seed 42 is not locked; the component claimed otherwise");
}
```

- [ ] **Step 2–5:** fail, implement, pass, gate, commit.

---

### Task 7: View — `tongue`

**START WITH A BLOCKING CHECK. Do not implement first and discover second.**

**Nothing outside `domains/language` and its own tests has ever called
`realize_tongue`.** This view would be its first consumer, so assembling a
`TongueGrammar`, a `Lexicon` and the pronoun map for a settlement is **unproven
territory**, not a wiring job.

**Files:**
- Create: `clients/game/bin/src/overture/tongue.rs`
- Test: in-module

**Interfaces:**
- `realize_common(spec: &Clause, vocab: &CommonVocabulary) -> String`
  (`domains/language/src/clause.rs:861`)
- `realize_tongue(clause: &Clause, grammar: &TongueGrammar, lexicon: &Lexicon,
  pronouns: &BTreeMap<&'static str, MorphForm>) -> Result<String, TongueGap>`
  (`domains/language/src/grammar.rs:621`)
- `hornvale_worldgen::{language_of_in (:5359), morph_options (:6979)}` — the
  driver already calls these for name resolution; read `driver.rs`'s use of them.

- [ ] **Step 0 (BLOCKING): prove the assembly path exists.**

Write a throwaway probe that, for seed 42's flagship settlement, obtains a
`TongueGrammar`, a `Lexicon` and the pronoun map and realizes **one** clause both
ways. Run it.

- **It works** → record the exact call chain in your report and continue.
- **It does not** → **reply `BLOCKED:` with what is missing.** Do not invent a
  path, do not fall back to Common-only prose, and do not widen a language API
  to make it work. The view is worth having only if the world can actually speak.

- [ ] **Step 1: Write the failing test**

```rust
#[test]
fn one_clause_is_realized_in_common_and_in_the_tongue() {
    // The interlinear: ONE structure, two realizations. If the two are equal
    // the tongue is not doing anything, which is the vacuity this test exists
    // to exclude.
    let (common, tongue) = tongue_view().sample(&full_world()).expect("a clause");
    assert!(!common.is_empty() && !tongue.is_empty());
    assert_ne!(common, tongue,
        "the tongue realization is identical to Common; no language was applied");
}

#[test]
fn the_tongue_is_silent_before_there_are_peoples() {
    assert!(!tongue_view().can_speak(BuildDepth::Terrain));
    assert!(tongue_view().can_speak(BuildDepth::Settlements));
}
```

- [ ] **Step 2–5:** fail, implement, pass, gate, commit. **A `TongueGap` is a
  legitimate outcome** — the view says so honestly rather than falling back.

---

### Task 8: The cache and its validity protocol

**Files:**
- Create: `clients/game/bin/src/cache.rs`
- Modify: `clients/game/bin/src/driver.rs` (`start`), `main.rs`
- Test: in-module

**Interfaces:**
- `World::{save(&Path), load(&Path), to_json, from_json}` (`kernel/src/world.rs:141,164,170,175`)
- `cli::streams::{stamp (:143), what_moved (:156), reload_notice (:173)}` —
  **already written and tested; do not reimplement.**

- [ ] **Step 1: Write the failing tests**

```rust
#[test]
fn a_cache_with_different_pins_is_refused() {
    let c = Cache::write(&tmp(), &world_with(default_pins())).unwrap();
    assert!(c.load_if_valid(&tmp(), seed(42), &other_pins()).is_none(),
        "a cache built under different pins was accepted");
}

#[test]
fn a_moved_label_names_itself_rather_than_flagging_staleness() {
    // Layer 2, and the reason it beats a hash: `what_moved` reports WHICH
    // label moved. An empty stamp makes no claim and must report NOTHING
    // moved — the opposite failure (reporting everything) is the one the
    // kernel's own doc warns about.
    let moved = cli_streams_what_moved(&then_labels(), &now_labels_with_bump());
    assert_eq!(moved, vec!["settlement/name"]);
    assert!(cli_streams_what_moved(&BTreeMap::new(), &now_labels()).is_empty(),
        "an empty stamp must claim nothing moved, not everything");
}

#[test]
fn the_prefix_tripwire_catches_a_change_no_label_records() {
    // Layer 3, and spec §10 says this one MAY FAIL. If an undeclared genesis
    // change does not disturb the astronomy prefix, report that plainly: the
    // honest conclusion is that layers 1-2 are the whole protocol.
    let cached = astronomy_prefix_of(&cached_world());
    let fresh = astronomy_prefix_regenerated(seed(42), &default_pins());
    assert_eq!(cached, fresh, "the tripwire must be quiet on an unchanged tree");
}
```

- [ ] **Step 2: Run to verify they fail.**

- [ ] **Step 3: Implement** the three layers in order — cheapest refusal first.

- [ ] **Step 4: MEASURE H2.** Time a cached start against a generated one,
  `--release`, load checked first.
  - **under 1 s** → H2 supported; state the number.
  - **1 s or more** → **H2 IS NULL.** Report it plainly, retune nothing, and say
    whether the cache is worth its complexity. The spec preregistered this.

- [ ] **Step 5: DISPOSE H3.** Mutate a genesis constant **without** bumping a
  label and confirm the tripwire reddens.
  - **It reddens** → H3 supported.
  - **It does not** → **H3 IS NULL, and that is a real possibility the spec
    names.** The honest report is that layers 1–2 are the whole protocol and the
    tripwire is theatre. Do not strengthen the tripwire to rescue it.

- [ ] **Step 6: `cargo fmt --all`, `make gate-commit`, commit.**

---

### Task 9: Artifacts, book, chronicle, retrospective

**Definition of Done for every merged plan (CLAUDE.md Process, decisions 0013,
0020) — not optional.**

- [ ] **Step 1: `make rebaseline`, then `git status` IMMEDIATELY** (see the OOM
  hazard in Global Constraints), then the drift check:

```bash
git diff --exit-code -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$')
```

Branch table, not a prediction: `docs/audits/` and `docs/digest/` moving is
EXPECTED (type-audit drifts on any `pub` boundary change; the decision index
drifts when a decision lands). **Anything under `book/src/domesday/`,
`book/src/gallery/` or an almanac moving → STOP.** That is a determinism finding.

- [ ] **Step 2: Decision records 0357–0366**, per spec §12. Derive the real list
  from what shipped — several rulings may be decision-worthy and some of §12's
  wording may no longer match. Read two recent records in `docs/decisions/` for
  the house format first.

- [ ] **Step 3: Chronicle** `book/src/chronicle/the-overture.md`, wired into
  `book/src/SUMMARY.md`. **Lead with what was measured, not predicted.** If H2 or
  H3 came back null, that is the headline.

- [ ] **Step 4: Retrospective** `docs/retrospectives/the-overture.md` — process
  lessons, not product.

- [ ] **Step 5: Registry** — flip `CLIENT-startup-is-silent` and repoint it; add
  rows for the held items **each carrying its measurement** (chronicle view;
  cache `GeneratedTerrain` 199 ms; cache the demography report 480 ms; the living
  world view; music and literature as **view slots**). A row with a number is
  worth several without.

- [ ] **Step 6: Freshness sweep + Confidence Gradient.** **Grep
  `book/src/open-questions.md` for this campaign's domains before concluding no
  bet moved** — that is the named common mistake, and the previous campaign made
  it and had to correct itself.

- [ ] **Step 7: `make gate-commit`, commit.** Do NOT submit to the merge queue —
  that is the controller's.
