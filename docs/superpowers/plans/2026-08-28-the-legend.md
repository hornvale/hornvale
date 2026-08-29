# The Legend Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> superpowers:subagent-driven-development (recommended) or
> superpowers:executing-plans to implement this plan task-by-task. Steps use
> checkbox (`- [ ]`) syntax for tracking.

**Goal:** Give the client a glyph vocabulary governed by one allocation rule
— creatures distinguishable, terrain legible — with the classification
written once and shared by every renderer.

**Architecture:** One classifier function in `windows/scene` produces an
ordinal band index plus a nominal class id and their legends. The scene
builder calls it (so `clients/atlas` and any future tile client get it
through the wire they already consume); `plate.rs` calls it directly (so the
plate keeps The Quadrat's exact mesh addressing). Clients bind class ->
presentation: glyph and ANSI in the TUI, RGB in atlas, sprites later.

**Tech Stack:** Rust 2024, std + `serde` + `libm` only in the workspace
(decision 0004). `clients/game` is OUTSIDE the workspace and carries
`crossterm` + `signal-hook`. No new external crates anywhere.

**Spec:** `docs/superpowers/specs/2026-08-28-the-legend-design.md`

## Global Constraints

- **No `HashMap`/`HashSet`** — `BTreeMap`/`BTreeSet`/`Vec` only, enforced by
  `clippy.toml` `disallowed-types`. Float sorting uses `total_cmp`.
- **No wall-clock time.** `Instant`/`SystemTime` are banned.
- **`clients/game/core` depends on NO hornvale crate** (the containment
  rule, The Quire spec §6). It reads the wire and nothing else. Do not add a
  path dependency to it — if a task seems to need one, the task is wrong.
- **`f64::sin`/`cos` are workspace-disallowed**; use
  `hornvale_kernel::math`. In `clients/game/core` neither is available —
  follow `chart.rs`'s existing scoped `#[allow(clippy::disallowed_methods)]`
  with its comment, do not invent a new remedy.
- **Every crate sets `#![warn(missing_docs)]`** — every public item, field
  and variant gets a one-line doc comment.
- **Additive only on `scene/*`, and floats stay.** A band index is added
  BESIDE `elevation_m`, never in place of it. `scene/tiles/v1` has a golden
  byte pin its own test calls "the epoch decision point": if it moves,
  STOP and escalate — that is an epoch decision, not a rebaseline.
- **`cargo fmt` is the last step before every commit.** fmt-gate skips are
  this project's most common review finding.
- Run `make gate-commit` before each commit. `clients/game` is outside it —
  see Task 0.
- **Run a suite ONCE and grep the captured file.** Never re-run to ask a
  second question: `cargo test ... > /tmp/hv.log 2>&1; echo "exit=$?"` then
  grep. A pre-commit hook enforces this.

---

### Task 0: Read this before Task 1

Not a work task. Four facts that will otherwise cost an hour each.

1. **`clients/game` is outside the cargo workspace and outside every gate.**
   `make gate-commit` does not build it. Its own checks are, from
   `clients/game/`: `cargo fmt --check`, then
   `cargo clippy --all-targets -- -D warnings`, then `cargo test`. Run them
   by hand for any task touching it. (`CLIENT-game-bin-has-no-gate` is the
   open registry row; this plan does not close it.)
2. **`clients/game/core/tests/` is NOT consolidated** into a single
   `suite.rs` the way workspace crates are — it has `cell.rs`, `chart.rs`,
   `plan.rs` and others as separate binaries. The test-binary ratchet
   freezes workspace crates and this one is outside. Follow the local
   convention; do not consolidate.
3. **The three renderers must not diverge.** `clients/game/core/src/
   chart.rs`, `clients/vessel/src/pane_chart.ts` and
   `windows/scene/src/surrounds_ascii.rs` already implement one projection
   rule three times. This campaign adds a shared VOCABULARY. Do not add a
   fourth independent implementation of it — if you find yourself writing a
   second `match` over the same classes, stop and say so in the report.
4. **A rung is a mesh depth (decision 0287) and a view may never invent
   detail below its datum (0196).** Any change that samples between mesh
   vertices violates both. That is why the classifier is shared as a
   FUNCTION and the plate does not fetch a scene — see spec §3.0.

---

### Task 1: The glyph register, and the guard that makes it a rule

The register is the campaign's spine: one table naming every character the
client may draw and what it means. The guard turns it from a convention into
a rule, and it goes FIRST because every later task adds entries.

**Files:**
- Create: `clients/game/core/src/register.rs`
- Modify: `clients/game/core/src/lib.rs` (add `pub mod register;`)
- Test: `clients/game/core/tests/register.rs`

**Interfaces:**
- Produces: `pub enum Population { Observer, Relief, Elevation, Water,
  Structure, Creature, PointSite, Chrome, Subterranean }`;
  `pub struct Binding { pub glyph: char, pub population: Population, pub
  means: &'static str }`; `pub const REGISTER: &[Binding]`;
  `pub fn binding_of(glyph: char) -> Option<&'static Binding>`.

- [ ] **Step 1: Write the failing test**

```rust
// clients/game/core/tests/register.rs
use hornvale_game_core::register::{Population, REGISTER, binding_of};

#[test]
fn no_character_is_bound_twice() {
    // THE campaign invariant. Before The Legend, `.` meant land, floor AND
    // relief-band-2; `+` meant threshold, "everything else" AND water; `#`
    // meant wall AND settlement. Nothing caught it because each pane
    // allocated privately. This is the thing that catches it.
    let mut seen: std::collections::BTreeMap<char, &str> =
        std::collections::BTreeMap::new();
    for b in REGISTER {
        if let Some(prior) = seen.insert(b.glyph, b.means) {
            panic!(
                "glyph {:?} is bound twice: {:?} and {:?}",
                b.glyph, prior, b.means
            );
        }
    }
}

#[test]
fn the_subterranean_region_is_reserved_and_empty() {
    // Held for Delving campaign 2 (MAP-underworld-chart). Reserved means the
    // POPULATION exists so a claim is a one-line addition, and that nothing
    // else may quietly take those marks first.
    assert!(
        !REGISTER
            .iter()
            .any(|b| b.population == Population::Subterranean),
        "the subterranean region is reserved for Delving 2; \
         claiming it is that campaign's call, not this one's"
    );
}

#[test]
fn every_binding_is_reachable_by_lookup() {
    for b in REGISTER {
        assert_eq!(binding_of(b.glyph).map(|f| f.glyph), Some(b.glyph));
    }
    assert!(binding_of('\u{1F600}').is_none());
}
```

- [ ] **Step 2: Run it and watch it fail**

From `clients/game/`: `cargo test -p hornvale-game-core --test register`
Expected: FAIL to compile — the `register` module does not exist.

That is a COMPILE failure, which proves nothing about the assertions. It is
acceptable here and ONLY here, because the module is new and there is no
live surface to capture a behavioural red from. Every later task captures a
behavioural red instead.

- [ ] **Step 3: Write the register**

```rust
// clients/game/core/src/register.rs
//! The glyph register: every character the client may draw, and what it
//! means. ONE table, because the alternative is what shipped before The
//! Legend — each pane allocating privately, and `.`, `+` and `#` each
//! meaning two or three different things with nothing to notice.
//!
//! The allocation rule (spec §2): a glyph carries ORDER (ink ascends with
//! the quantity) or IDENTITY (the character is the referent's initial),
//! never an arbitrary category. Colour carries category and MAY FAIL, so
//! nothing a reader must trust lives only there.

/// Which population of referents a glyph belongs to. A character belongs to
/// exactly one, which is what makes double-binding detectable.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Population {
    /// The possession's own cell.
    Observer,
    /// Walk-band impedance: how hard this ground is to cross.
    Relief,
    /// Globe-scale elevation: how high this ground stands.
    Elevation,
    /// Surface water, by kind.
    Water,
    /// Built structure — wall, threshold.
    Structure,
    /// A living thing, drawn as its noun's initial.
    Creature,
    /// A discovered point site — settlement, cave mouth.
    PointSite,
    /// Interface furniture that is not part of the world.
    Chrome,
    /// RESERVED for Delving campaign 2. Deliberately unpopulated.
    Subterranean,
}

/// One character's single meaning.
#[derive(Debug)]
pub struct Binding {
    /// The character drawn.
    pub glyph: char,
    /// The population it belongs to.
    pub population: Population,
    /// What it means — for the legend, and for the double-binding panic.
    pub means: &'static str,
}

/// Every character the client may draw. Adding a row is how a campaign
/// claims a mark; the guard in `tests/register.rs` refuses a second claim on
/// a character already spoken for.
///
/// `Creature` is NOT enumerated: a creature draws its noun's initial, so its
/// codespace is `a-z`/`A-Z` by RULE rather than by row. Those letters are
/// therefore unavailable to every other population, which is exactly the
/// constraint the rule intends.
pub const REGISTER: &[Binding] = &[
    Binding { glyph: '@', population: Population::Observer, means: "you" },
    Binding { glyph: '>', population: Population::Chrome, means: "the command prompt" },
];

/// The binding for `glyph`, if the register claims it.
pub fn binding_of(glyph: char) -> Option<&'static Binding> {
    REGISTER.iter().find(|b| b.glyph == glyph)
}
```

- [ ] **Step 4: Run the tests**

From `clients/game/`: `cargo test -p hornvale-game-core --test register`
Expected: PASS, 3 tests.

- [ ] **Step 5: Verify the guard catches the thing it exists for**

Temporarily add a second `Binding` for `'@'` with a different `means`,
re-run, and confirm `no_character_is_bound_twice` FAILS naming both
meanings. Then remove it.

**Mandatory, and not ceremony.** A guard nobody has seen fail is a guard
nobody has evidence works; this project has a documented case of a mutation
test that matched nothing and produced a green that looked robust. Assert to
yourself that you saw the red, and say so in your report.

- [ ] **Step 6: fmt, check, commit**

From `clients/game/`: `cargo fmt`, then
`cargo clippy --all-targets -- -D warnings`, then `cargo test`.
Then from the repo root:

```bash
git add clients/game/core/src/register.rs clients/game/core/src/lib.rs clients/game/core/tests/register.rs
git commit -m "feat(legend): the glyph register, and the guard that makes it a rule"
```

---

### Task 2: Ratify the channel contract as a decision record

**Files:**
- Create: `docs/decisions/NNNN-a-glyph-carries-order-or-identity-never-category.md`
- Modify: `book/src/frontier/idea-registry.md`

**Interfaces:**
- Produces: a decision number later tasks cite in doc comments.

- [ ] **Step 1: Claim the next number**

```bash
ls docs/decisions/ | sed 's/-.*//' | sort -n | tail -1
```

**Decision rule, and the obvious version of it is BACKWARDS.** An earlier
draft of this step said "a gap is what reddens, a collision is what wastes a
campaign". The opposite is true, and `cli/tests/suite/docs_consistency.rs`
says so at length in its own doc comment:

- **Contiguity above 0001 is NO LONGER ASSERTED.** Under parallel campaigns
  a hole is a NORMAL outcome — a campaign renumbers away from a collision or
  withdraws a draft — so the check "lost its discriminating power". Main has
  five gaps right now and is green.
- **Duplicates ARE guarded**, by `decision_numbers_are_unique`. A COLLISION
  is the red gate, and it is the failure that corrupts a citation handle.
- The retrospective that killed the contiguity half named this exact trap:
  *"the gap check pushes an author into the collision the uniqueness check
  exists to catch"* — requiring next-free makes you take the number another
  campaign is most likely holding.

So: **take a number safely above every live claim; do not fight for
next-free.** Campaigns reserve BLOCKS (The Overture's spec claims 0357-0366),
and a reserved block shows up as a gap on main, not as a file. Check
`git log --all --oneline -- docs/decisions/`, the board, and live campaign
specs' "Decision block:" lines. Leaving a gap costs nothing.

- [ ] **Step 2: Write the record**

Read `docs/decisions/0287-a-zoom-rung-is-a-mesh-depth.md` first — it is the
closest neighbour and this record cites it. State:

- **Context:** three characters already carried two or three meanings each,
  verified by reading every glyph constant; the table is spec §1.
- **Decision:** glyph carries order or identity, never arbitrary category;
  colour carries category and may fail; weight carries attention; a
  character means one thing across the whole client; two panes may use
  different ladders for different quantities but never the same character
  for two meanings.
- **Why the 22-glyph rejection is narrower than it reads:** the defect was
  NOMINALITY, not count. Ordinal marks self-legend; nominal marks never do.
  This supersedes nothing — `CLIENT-glyphs-22-rejected` stays rejected — it
  states the principle that rejection was an instance of.
- **Consequences:** `CLIENT-four-channels` promoted from `raw`; the reserved
  `Subterranean` region named, with Delving 2 as its claimant.

- [ ] **Step 3: Promote the registry row**

Set `CLIENT-four-channels`' Status to `ratified (NNNN)` and its Where to
cite the new record. Follow `CLIENT-two-tier-position`, which already reads
`ratified (0069)`.

- [ ] **Step 4: Regenerate and read the drift check**

```bash
make rebaseline
git diff --exit-code -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$')
```

**Branch table:**
- Only `docs/digest/` moved -> expected; the in-force decision index drifts
  whenever a decision is added. `git add` it in this commit.
- `docs/audits/type-audit-report.md` also moved -> you added a `pub` item;
  fine, add it.
- `book/src/domesday/` or `book/src/gallery/` moved -> STOP. Those track the
  census and nothing here should touch it. Escalate.

- [ ] **Step 5: Gate and commit**

```bash
make gate-commit
git add docs/decisions/ book/src/frontier/idea-registry.md docs/digest/
git commit -m "decide(NNNN): a glyph carries order or identity, never an arbitrary category"
```

---

### Task 3: The shared classifier

The campaign's load-bearing unit. One function, called by the scene builder
(Task 4) and the plate (Task 6), so a class can never be defined twice.

**Files:**
- Create: `windows/scene/src/classify.rs`
- Modify: `windows/scene/src/lib.rs` — add `mod classify;` followed by
  `pub use classify::*;`. **NOT `pub mod`.** This crate declares every
  module privately and glob-re-exports it (`mod region; pub use region::*;`
  at lib.rs:18-19), so its public surface is FLAT. Following the wrong
  convention here makes the test's import path wrong too.
- Test: `windows/scene/tests/suite/classify.rs`, registered in
  `windows/scene/tests/suite.rs`

**Interfaces:**
- Consumes: `hornvale_terrain::WaterKind`.
- Produces: `pub fn elevation_band(elevation_m: f64, sea_level_m: f64) -> u8`
  (0..=4, ascending); `pub const ELEVATION_LEGEND: [&str; 5]`;
  `pub fn water_class(kind: WaterKind) -> u8`;
  `pub const WATER_LEGEND: [&str; 4]`.

- [ ] **Step 1: Write the failing tests**

```rust
// windows/scene/tests/suite/classify.rs
// FLAT import, not `hornvale_scene::classify::...` — the crate glob-
// re-exports its private modules, and its existing tests import this way.
use hornvale_scene::{ELEVATION_LEGEND, WATER_LEGEND, elevation_band, water_class};
use hornvale_terrain::WaterKind;

#[test]
fn the_elevation_band_is_monotone_in_elevation() {
    // The ORDINAL property, which is the whole reason a ladder reads without
    // a legend (decision NNNN). If this is not monotone, ink density stops
    // meaning "higher" and the ladder becomes a nominal set — the thing
    // CLIENT-glyphs-22-rejected rejected.
    let sea = 0.0;
    let mut last = elevation_band(-5000.0, sea);
    for m in (-5000..=9000).step_by(50) {
        let b = elevation_band(f64::from(m), sea);
        assert!(
            b >= last,
            "band fell from {last} to {b} at {m} m — the ladder is not ordinal"
        );
        last = b;
    }
}

#[test]
fn the_elevation_band_spans_its_whole_legend() {
    // A ladder whose top rung is unreachable has fewer rungs than it claims,
    // and the specimen sheet would be measuring a fiction.
    let sea = 0.0;
    let seen: std::collections::BTreeSet<u8> = (-5000..=9000)
        .step_by(10)
        .map(|m| elevation_band(f64::from(m), sea))
        .collect();
    assert_eq!(
        seen.len(),
        ELEVATION_LEGEND.len(),
        "bands actually reached: {seen:?} against {} legend entries",
        ELEVATION_LEGEND.len()
    );
}

#[test]
fn sea_level_is_the_datum_not_the_number_zero() {
    // A world whose sea level is not 0 m must band identically to one whose
    // is, for the same HEIGHT ABOVE SEA. Hard-coding 0.0 passes every seed-42
    // test and silently mis-bands every other world.
    for offset in [-800.0, 0.0, 1200.0] {
        assert_eq!(
            elevation_band(offset + 500.0, offset),
            elevation_band(500.0, 0.0),
            "banding moved when sea level moved to {offset}"
        );
    }
}

#[test]
fn every_water_kind_has_a_distinct_class_and_a_legend_entry() {
    let kinds = [
        WaterKind::Ocean,
        WaterKind::SaltBasin,
        WaterKind::River,
        WaterKind::DryLand,
    ];
    let classes: std::collections::BTreeSet<u8> =
        kinds.iter().copied().map(water_class).collect();
    assert_eq!(classes.len(), kinds.len(), "two water kinds collapsed to one class");
    for c in classes {
        assert!((c as usize) < WATER_LEGEND.len());
    }
}
```

- [ ] **Step 2: Capture a BEHAVIOURAL red, not a compile error**

First run cannot fail behaviourally — the type does not exist. So write the
SIMPLEST WRONG implementation, `elevation_band` returning a constant `0`,
and run:

`cargo test -p hornvale-scene --test suite -- classify > /tmp/hv.log 2>&1`
then grep `/tmp/hv.log`.

Expected: the monotone test PASSES (a constant is trivially monotone) and
`the_elevation_band_spans_its_whole_legend` FAILS.

**That asymmetry is the point and you must observe it.** It proves the span
test carries the weight and that the monotone test alone would have admitted
a constant. Record both outcomes in your report.

- [ ] **Step 3: Implement**

```rust
// windows/scene/src/classify.rs
//! THE classifier. One definition of what a piece of ground IS, called by
//! the tile-scene builder and by the game client's plate, so the two can
//! never disagree.
//!
//! Why a function and not a wire field the plate reads back: `RegionScene`
//! is a CUBE-FACE quadtree that barycentrically resamples geosphere values
//! onto its nodes, and the plate addresses the geosphere mesh directly. A
//! plate fetching that scene would resample twice, violating decision 0287
//! (a tile IS a facet) and 0196 (never invent detail below the datum). See
//! The Legend spec §3.0.

/// The five elevation rungs, ascending. The index is [`elevation_band`]'s
/// return; the TUI binds these to ink, atlas binds them to colour.
pub const ELEVATION_LEGEND: [&str; 5] =
    ["abyssal", "lowland", "upland", "highland", "montane"];

/// Metres above sea level at which each band STARTS, ascending. Band 0 is
/// everything below `BAND_FLOORS_M[0]`.
const BAND_FLOORS_M: [f64; 4] = [0.0, 400.0, 1200.0, 2800.0];

/// Which [`ELEVATION_LEGEND`] rung `elevation_m` falls in, given this
/// world's own `sea_level_m`.
///
/// **Ordinal and total.** Monotone non-decreasing in `elevation_m`, so ink
/// density may carry it (decision NNNN). Sea level is the DATUM, never the
/// number zero — a world whose sea level is 1,200 m bands identically to one
/// at 0 m for the same height above sea.
pub fn elevation_band(elevation_m: f64, sea_level_m: f64) -> u8 {
    let asl = elevation_m - sea_level_m;
    let mut band = 0u8;
    for floor in BAND_FLOORS_M {
        if asl >= floor {
            band += 1;
        }
    }
    band
}

/// The four water classes, in [`WATER_LEGEND`] order.
pub const WATER_LEGEND: [&str; 4] = ["ocean", "salt basin", "river", "dry"];

/// Which [`WATER_LEGEND`] class a [`hornvale_terrain::WaterKind`] is.
///
/// NOMINAL, not ordinal — "river" is not more or less than "ocean" — so
/// under decision NNNN this rides colour, and only the ocean/dry split
/// (a boundary the reader must trust) reaches a glyph.
pub fn water_class(kind: hornvale_terrain::WaterKind) -> u8 {
    match kind {
        hornvale_terrain::WaterKind::Ocean => 0,
        hornvale_terrain::WaterKind::SaltBasin => 1,
        hornvale_terrain::WaterKind::River => 2,
        hornvale_terrain::WaterKind::DryLand => 3,
    }
}
```

**Do not tune `BAND_FLOORS_M` to make a picture you like.** If Task 5's
specimen sheet shows the bands badly distributed on real worlds, that is a
FINDING to report with the distribution measured, and the constant moves in
its own commit that says so. Retuning silently after unblinding is exactly
what the preregistration discipline exists to prevent.

- [ ] **Step 4: Run the tests**

`cargo test -p hornvale-scene --test suite -- classify > /tmp/hv.log 2>&1`
Expected: PASS, 4 tests.

- [ ] **Step 5: fmt, gate, commit**

```bash
cargo fmt && make gate-commit
git add windows/scene/src/classify.rs windows/scene/src/lib.rs windows/scene/tests/
git commit -m "feat(scene): one classifier for what a piece of ground is"
```

---

### Task 4: The scene builder calls the classifier — and moves no bytes

H1's byte-identity half. Its own task because it is the one place a reviewer
can reject "the extraction changed behaviour" independently.

**Files:**
- Modify: `windows/scene/src/region.rs` (`RegionScene`, `tiles_region_scene_in`)
- Test: `windows/scene/tests/suite/golden.rs` (existing — RUN it, do not edit)

**Interfaces:**
- Consumes: Task 3's `elevation_band`, `ELEVATION_LEGEND`.
- Produces: `RegionScene` gains `pub elevation_band: Vec<u8>` and
  `pub elevation_legend: Vec<String>`, ADDED BESIDE `elevation_m`.

- [ ] **Step 1: Record the pre-change state**

`cargo test -p hornvale-scene --test suite -- golden > /tmp/hv-base.log 2>&1`
and keep the log.

- [ ] **Step 2: Add the fields additively**

Add to `RegionScene` immediately AFTER `elevation_m` — field order is JSON
key order and is contract, so never reorder and never insert before an
existing field:

```rust
    /// Elevation band per node — [`crate::elevation_band`]'s rung,
    /// BESIDE the raw metres rather than instead of them. A tile client wants
    /// the float for shading even once the band exists (The Legend spec §3.1).
    pub elevation_band: Vec<u8>,
    /// The elevation catalog, stable ascending order.
    pub elevation_legend: Vec<String>,
```

Populate from the same `elevation_m` values the builder already computes,
and extend the struct's `type-audit:` tag with
`bare-ok(index: elevation_band), bare-ok(identifier-text: elevation_legend)`.

- [ ] **Step 3: Run the golden and read the result against this branch table**

`cargo test -p hornvale-scene --test suite -- golden > /tmp/hv.log 2>&1`

- **Fails because the fixture gained two KEYS and no existing value changed**
  -> expected and correct. Rebaseline with `make rebaseline-goldens`, then
  `git diff` the fixture and confirm by eye that every pre-existing key holds
  its previous value.
- **Any pre-existing value changed** -> STOP. That is not additive. Do NOT
  rebaseline. You have found either a reordering or a classifier computing
  something different from the raw field. Report it.
- **`scene/tiles/v1`'s own byte pin moved** (as distinct from
  `tiles-region`) -> STOP and escalate. Its test calls it "the epoch decision
  point" and this task has no business moving it.

- [ ] **Step 4: Prove `elevation_m` is untouched**

```bash
git diff -- windows/scene/tests/fixtures/ | grep -E '^-' | grep -i elevation_m | head
```

Expected: NO output. A removed `elevation_m` line is the `scene/eclipses`
v1->v2 failure repeating — a bump that quietly dropped float fields, vetted
as correct and caught only by the gate.

- [ ] **Step 5: fmt, gate, commit**

```bash
cargo fmt && make gate-commit
git add windows/scene/
git commit -m "feat(scene): emit the elevation band beside the metres, never instead"
```

---

### Task 5: The specimen sheet

Selection happens against a rendered sheet in the medium the glyphs ship in
— the method that rejected the 22-glyph set. Deliberately NOT a browser
mockup: the judgment is "tellable apart at a glance in an 80x24 terminal".

**Files:**
- Create: `clients/game/bin/examples/specimen_sheet.rs`
- Create: `docs/audits/glyph-specimen-sheet.txt`
- Modify: `docs/generated-paths.txt`, `scripts/regenerate-artifacts.sh`

**Interfaces:**
- Consumes: Task 1's `REGISTER`, Task 3's legends.

- [ ] **Step 1: Write the example**

It renders to stdout, at exactly 80x24: every candidate elevation ladder and
every candidate impedance ladder side by side; each in monochrome and again
with colour; the creature-initial rule over a real species roster; and the
register's full table as a legend.

It must take no arguments and **must not read stdin** — an agent's stdin is
at EOF, so a stdin-reading command passes here and hangs for a human at a
terminal.

- [ ] **Step 2: Declare the artifact BEFORE generating it**

Add **ONE** line to `docs/generated-paths.txt`:

```
docs/audits/glyph-specimen-sheet.txt
```

`docs/audits/` is ALREADY declared, at line 50 — do not add it a second
time. Lines 55-64 already carry this exact convention, with its explanatory
comment, for files declared by name inside an already-declared directory;
follow that comment style rather than inventing one.

**The directory alone is not enough, and the trap has a name.**
`git diff --exit-code` against a path with no index entry is silently
VACUOUS, and `docs/audits/`'s other tracked files keep the tracked-ness
check green while the new file is invisible. Declaring the FILE by name
makes the check refuse until it is `git add`-ed — an observed refusal rather
than a hoped-for one (The Stope, Task 2b).

- [ ] **Step 3: Wire it into regeneration**

Add to `scripts/regenerate-artifacts.sh` in the clients section, following
the surrounding style. **The `>` REDIRECT writes the file, not the command**
— a bare `cargo run` regenerates nothing, and the drift check then reports an
empty diff that reads as "no drift".

- [ ] **Step 4: Generate, and confirm the guard refuses first**

```bash
make rebaseline
git diff --exit-code -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$')
```

Expected: it REFUSES, naming the untracked sheet. Observe that refusal — it
is the proof Step 2 worked. Then `git add` the sheet and re-run; expected
clean.

- [ ] **Step 5: Read the sheet and report**

Open it in an 80-column terminal. Report which ladders are tellable apart
and which are not, quoting the sheet's own output. **Your recommendation is
the input to Task 6's ladder and it may contradict this plan.** If it does,
say so — the sheet is the authority here, not the plan.

- [ ] **Step 6: Commit**

From `clients/game/`: `cargo fmt`, then
`cargo clippy --all-targets -- -D warnings`. Then from the root:

```bash
git add clients/game/bin/examples/specimen_sheet.rs docs/audits/glyph-specimen-sheet.txt docs/generated-paths.txt scripts/regenerate-artifacts.sh
git commit -m "feat(legend): the specimen sheet, rendered where the glyphs ship"
```

---

### Collision resolutions — binding on Tasks 6 through 10

The pre-flight scan found that the register's guard would fire on three
characters that are ALREADY double-bound today, and the plan as first
written said what would catch them and not what to do. Settled, so no task
has to decide mid-flight:

- **`.` — one binding, meaning "ordinary traversable ground".** The chamber
  floor (`plan.rs:69`) and walk-band relief 2 are the same concept at two
  scales, not two meanings. Nothing moves, and the impedance ladder stays
  byte-identical to the sim's, which an agreement test pins.
- **`#` — the SETTLEMENT glyph moves; WALL keeps `#`.** A wall you cannot
  pass is not a town, and there is no honest single meaning. Wall-is-`#` is
  universal convention and lives in three renderers
  (`lattice/render.rs::WALL`, `plan.rs`, `pane_plan.ts`); the settlement
  glyph is drawn in one place (`plate.rs`'s feature layer) and is younger.
  Move the cheaper one. Task 7 picks the replacement from the specimen
  sheet.
- **`+` — means THRESHOLD, sole claimant.** Task 8 deletes `chart.rs`'s
  `+`. **Task 6 must NOT hand `+` to any water class.**

The register governs the CLIENT's renderers. `surrounds_ascii.rs` is
sim-side and outside it; it must AGREE where it draws the same quantity
(Task 8 preserves that by reusing its exact ladder), but its glyphs are not
register rows — that would put a client concern in `windows/`.

---

### Task 6: The world map draws elevation and water

**Files:**
- Modify: `clients/game/bin/src/plate.rs` (`TileTerrain`, `terrain_at_tile`,
  `draw_with`, and the glyph constants at 184-187)
- Modify: `clients/game/core/src/register.rs`
- Test: `clients/game/bin/tests/plate_vocabulary.rs`

**Interfaces:**
- Consumes: `hornvale_scene::classify::{elevation_band, water_class}`,
  Task 1's `binding_of`.
- Produces: `TileTerrain` gains `pub band: u8` and `pub water: u8`;
  `pub ocean: bool` is RETAINED — the strip's invariant depends on it.

- [ ] **Step 1: Write the failing tests**

```rust
// clients/game/bin/tests/plate_vocabulary.rs

#[test]
fn seed_42_draws_more_than_two_distinct_terrain_glyphs() {
    // Nathan's actual report: "the ocean is ~ and the land is . -- nothing
    // more detailed than that". This is that sentence as an assertion.
    let grid = /* draw seed 42 at band B, 80x24, colour off */;
    let glyphs: std::collections::BTreeSet<char> =
        /* every terrain-layer glyph in `grid` */;
    assert!(glyphs.len() > 2, "the whole terrain vocabulary is still {glyphs:?}");
}

#[test]
fn every_drawn_glyph_is_claimed_by_the_register() {
    // Decision NNNN enforced against a REAL render rather than the table
    // alone: a glyph nobody claimed is exactly how `+` came to mean three
    // things.
    let grid = /* draw seed 42 at band B, 80x24, colour off */;
    for g in /* every glyph in `grid` */ {
        assert!(
            hornvale_game_core::register::binding_of(g).is_some()
                || g.is_ascii_alphabetic() // creature initials, claimed by rule
                || g == ' ',
            "{g:?} is drawn but unclaimed"
        );
    }
}

#[test]
fn the_ocean_land_boundary_still_agrees_with_the_terrain() {
    // The Portolan fix round 1, Finding 2 bought this with a 49-point vote:
    // the strip must never name a land feature on a cell drawn as water.
    /* for each tile: assert terrain.is_ocean(t.vertex) == t.ocean */
}
```

Fill the `/* ... */` bodies from `plate.rs`'s existing tests — a harness
already exists and its fixtures are the right ones. Do not invent a second.

- [ ] **Step 2: Run and capture the behavioural red**

From `clients/game/`:
`cargo test --test plate_vocabulary > /tmp/hv.log 2>&1`

Expected: `seed_42_draws_more_than_two_distinct_terrain_glyphs` FAILS
reporting exactly `{'.', '~'}`. **That is the campaign's occasioning defect
reproduced as a test.** Quote the failure in your report.

- [ ] **Step 3: Implement**

Resolve the class in `terrain_at_tile` by calling Task 3's functions at the
tile's own `vertex` — the vertex the function already computes. Keep the mesh
addressing EXACTLY as it is: no new search, no resampling, no scene fetch
(decisions 0287, 0196). Bind class -> glyph through the register, using the
ladder Task 5's sheet selected.

- [ ] **Step 3b: Decide derive-once ON EVIDENCE, not on the spec's say-so**

Spec §7 argues the win is DERIVE-ONCE: classify each of the ~40,962 vertices
into a `VertexMap<u8>` once per world, so every rung and redraw is a `Vec`
index instead of a per-tile computation.

**Do not implement that on the argument alone.** `elevation_band` is a
handful of float comparisons and The Quadrat's tile cache already memoizes
the result, so the per-tile cost may be unmeasurable. Measure first:

- Per-tile classification is **not** measurable against Task 11's bar ->
  leave it per-tile. Say so in your report, and note that spec §7's
  mechanism went unused because it was unnecessary — that is a finding.
- It IS measurable -> precompute into `hornvale_kernel::geosphere::VertexMap`
  via `VertexMap::from_fn`, which is ALREADY `Vec`-backed and dense-indexed
  by `Vertex` (`geosphere.rs:47`). This is the kernel's existing derive-once
  idiom, the same one it names for `Fbm`.

**Either way, do NOT expand `kernel/src/component.rs`.** Its dense-`Vec`
backend is deferred to the ECS program and its trigger (spec §5) is a class
needing a FACET key rather than a vertex key. Terrain is vertex-keyed, so
this plan does not reach that trigger. If you believe it does, STOP and
escalate — that is ECS substrate work and needs a board notice and its own
stage boundary, not a step inside this task.

- [ ] **Step 4: Run the tests, plus the two that must NOT move**

Run the client suite once, capture, grep. Then, separately, the two
agreement pins:
`mesh_addressing_agrees_with_the_spatial_search` (hornvale-scene) and
`the_shape_matches_the_sims_own_ascii_render` (hornvale-game-core).

Expected: all PASS. If either pin moves, the addressing changed — that is a
defect, not a rebaseline.

- [ ] **Step 5: fmt, check, commit**

From `clients/game/`: `cargo fmt`, `cargo clippy --all-targets -- -D warnings`,
`cargo test`. Then from the root: `make gate-commit`.

```bash
git add clients/game/
git commit -m "feat(legend): the world map draws elevation and water, not land and not-land"
```

---

### Task 7: The world map draws landforms

**Files:**
- Modify: `clients/game/bin/src/plate.rs` (the FEATURE layer)
- Modify: `clients/game/core/src/register.rs`
- Test: `clients/game/bin/tests/plate_vocabulary.rs` (extend)

**Interfaces:**
- Consumes: `GeneratedTerrain::{has_edifice, nearest_boundary_at,
  waterfalls, deltas, playas}`.

- [ ] **Step 1: Write the failing test**

```rust
#[test]
fn seed_42_draws_at_least_one_of_each_landform_it_actually_has() {
    // Guards a glyph that is defined and unreachable — the defect The
    // Quadrat found, where seed 42's flagship settlement was undrawable at
    // every shipped rung. For each landform the terrain reports as PRESENT
    // on seed 42, assert its glyph appears somewhere across the shipped
    // rungs. A landform seed 42 does not have is SKIPPED, not failed.
}
```

- [ ] **Step 2: Run, expect a behavioural red**

Expected: FAIL — no landform glyph is drawn anywhere today.

- [ ] **Step 3: Implement in the FEATURE layer, not the terrain layer**

Landforms are point sites: they PROJECT from their own committed coordinate
rather than being sampled, exactly as settlements and caves have since The
Quadrat. Sampling them reproduces the defect where a site appeared only if an
area-majority happened to land on its vertex. The feature layer is cached on
the discovery version; keep that key.

- [ ] **Step 4: Run, then commit**

From `clients/game/`: fmt, clippy, test. Then `make gate-commit` from root.

```bash
git add clients/game/
git commit -m "feat(legend): volcanoes, ranges, waterfalls, deltas and playas reach the map"
```

---

### Task 8: The walk band gets the impedance ladder

**Files:**
- Modify: `clients/game/core/src/chart.rs` (`PLACED_GLYPH` at 79, `glyph_of`)
- Modify: `clients/game/core/src/register.rs`
- Test: `clients/game/core/tests/chart.rs` (extend)

**Interfaces:**
- Consumes: the wire's `SurroundsCell.relief` (`u32`, an index into
  `relief_legend`) and `SurroundsCell.micro` (`openness`, `relief`).

**Containment:** `clients/game/core` depends on NO hornvale crate. `relief`,
`relief_legend` and `micro` are ALREADY on `scene/surrounds/v2`, so this
needs no dependency. If you find yourself wanting `hornvale_scene`, stop —
the data is on the wire.

- [ ] **Step 1: Write the failing tests**

```rust
#[test]
fn the_walk_band_draws_an_ordinal_ladder_not_one_glyph() {
    // chart.rs:79's PLACED_GLYPH is `+` for "terrain and marks alike".
    // The fixture `chart.rs` already includes:
    //   include_str!("fixtures/session-seed-42-turn-0.json")
    // It is asserted to be a WALK-band turn by that file's own setup, which
    // panics on a Chamber spatial — so it is the right one, and there is no
    // need to build a second harness.
    let grid = /* render that fixture at 80x24, colour off */;
    let terrain_glyphs: std::collections::BTreeSet<char> = /* ... */;
    assert!(terrain_glyphs.len() > 1, "still one glyph: {terrain_glyphs:?}");
}

#[test]
fn the_ladder_ascends_with_impedance() {
    // Ordinality on the RENDERED output, not on the helper: a ladder that is
    // ordinal in a private function and shuffled at the call site reads as
    // nominal, which is what decision NNNN forbids.
}
```

- [ ] **Step 2: Run, capture the red**

Expected: the first FAILS reporting a single-element set.

- [ ] **Step 3: Implement**

Port the impedance ladder from `windows/scene/src/surrounds_ascii.rs`
(`impedance_glyph`, `relief + 0.5*canopy + 0.5*roughness`). **Use the SAME
characters** — this is the same quantity at the same scale in a second
renderer, so sharing them is required, not forbidden. Claim them in the
register under `Population::Relief`.

Contrast with Task 6: the world map's ELEVATION ladder is a DIFFERENT
quantity and must use different characters (spec §2.2). Same rule, opposite
conclusion, because the quantity differs there and matches here.

- [ ] **Step 4: Verify the renderers still agree**

Run the client suite once and grep; then
`the_shape_matches_the_sims_own_ascii_render`.

- [ ] **Step 5: Commit**

```bash
git add clients/game/
git commit -m "feat(legend): the walk band reads impedance, not plus signs"
```

---

### Task 9: Creatures become distinguishable

Closes the coverage audit's FIRST UNMET item, 2.1 Entities and Components
(`absent`): *"a creature and a boulder are the same character, on every
seed, with no flag that changes it."*

**Files:**
- Modify: `clients/game/core/src/lexicon.rs` (the derivation)
- Modify: `clients/game/core/src/chart.rs`, `clients/game/core/src/plan.rs`
- Modify: `clients/game/bin/src/plate.rs` (`mark_glyph` at 668)
- Test: `clients/game/core/tests/lexicon.rs`

**Interfaces:**
- Consumes: `Mark.noun` (`String`), `PlanMark.noun`.
- Produces: `pub fn creature_glyph(noun: &str, rank: usize) -> char`.

- [ ] **Step 1: Write the failing tests**

```rust
#[test]
fn a_goblin_and_a_bugbear_draw_differently() {
    assert_ne!(creature_glyph("goblin", 0), creature_glyph("bugbear", 0));
}

#[test]
fn the_glyph_is_the_nouns_own_initial() {
    // IDENTITY, per decision NNNN: the character spells the thing, which is
    // why it needs no legend.
    assert_eq!(creature_glyph("goblin", 0), 'g');
    assert_eq!(creature_glyph("bugbear", 0), 'b');
}

#[test]
fn a_collision_resolves_deterministically_and_never_by_an_authored_table() {
    // goblin and gnoll both want `g`. Rank is registry order.
    let a = creature_glyph("goblin", 0);
    let b = creature_glyph("gnoll", 1);
    assert_ne!(a, b);
    assert_eq!(creature_glyph("gnoll", 1), b, "not deterministic");
}

#[test]
fn a_fifteenth_people_needs_no_edit() {
    // The Radiation moved the roster 9 -> 15 on 2026-08-27. An authored table
    // would have gone stale that day (MAP-derivation-outlives-its-wiki).
    assert!(creature_glyph("an-unheard-of-people", 7).is_ascii_alphabetic());
}

#[test]
fn a_noun_with_no_ascii_initial_still_draws_something() {
    // Infallible by design: a render must not panic on a document that
    // parsed — the rule chart.rs's `weight_of` already follows.
    assert!(!creature_glyph("\u{4e2d}\u{6587}", 0).is_whitespace());
}
```

- [ ] **Step 2: Verify the DEFECT behaviourally before writing code**

The function does not exist, so the suite gives a compile error, which
proves nothing. Observe the live defect instead: run the game on seed 42 and
confirm by eye that every creature draws as one mark. Quote it in your report
— this is the audit's `absent` verdict OBSERVED rather than cited.

- [ ] **Step 3: Implement, and wire all three call sites**

`plate.rs:668`'s `mark_glyph` switches on `kind`, which only ever carries
`"settlement"`, `"cave"` and `"agent"` — that is why every creature is `&`.
Switch on the NOUN for the agent case; leave `"settlement"` and `"cave"`
alone (they are point sites, not creatures).

`plan.rs`'s marks pass is currently a structural no-op that redraws the same
glyph — its own doc says *"Marks carry no glyph of their own this
campaign."* This is that campaign.

- [ ] **Step 4: Run everything, then commit**

From `clients/game/`: fmt, clippy, test (capture once, grep). Then
`make gate-commit` from the root.

```bash
git add clients/game/
git commit -m "feat(legend): a goblin is not a boulder"
```

---

### Task 10: The hearth reaches the wire

The one task touching `windows/`. Furnishing anchors are derived and placed
(The Hearth) but never emitted: `PlanMark` is built only from creatures found
by sight.

**Files:**
- Modify: `windows/vessel/src/session.rs` (the sighting-to-mark path near
  1457-1480)
- Modify: `clients/game/core/src/register.rs`
- Test: `windows/vessel/tests/suite/` (follow the local file convention)

**Interfaces:**
- Consumes: the anchor graph The Hearth embeds into chamber cells.
- Produces: `PlanMark`s carrying `kind: "furnishing"`.

**Additive by design:** `Mark.kind`'s own doc says *"A consumer that does not
recognize a kind should still render the mark... a future kind needs no
special case anywhere to appear."* So this is a new `kind` value and no
special case anywhere — if you find yourself adding one, re-read that doc.

- [ ] **Step 1: Write the failing tests**

```rust
#[test]
fn a_chamber_with_a_hearth_emits_a_furnishing_mark() {
    // Sight-gated exactly as a creature mark is (`s.lit.contains(&cell)`).
}

#[test]
fn an_unlit_hearth_is_not_emitted() {
    // The negative half, and the one that would actually leak. Its own test
    // because a single positive-only test passes on an implementation that
    // emits every anchor unconditionally.
}
```

- [ ] **Step 2: Run, capture the red**

`cargo test -p hornvale-vessel --test suite -- furnishing > /tmp/hv.log 2>&1`
Expected: FAIL — no furnishing mark is emitted.

- [ ] **Step 3: Implement, sight-gated**

- [ ] **Step 4: Regenerate — the wire moved, so fixtures move**

```bash
make rebaseline
git diff --exit-code -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$')
```

**Branch table:**
- `clients/game/core/tests/fixtures/` moved (the committed seed-42 session
  snapshots) -> expected; a new mark kind is exactly what those witness.
  `git add`.
- `docs/audits/type-audit-report.md` moved -> expected if you added a `pub`
  item. `git add`.
- `book/src/domesday/` moved -> STOP. That tracks the census; nothing here
  should touch it.

- [ ] **Step 5: Gate and commit**

```bash
cargo fmt && make gate-commit
git add windows/vessel/ clients/game/
git commit -m "feat(vessel): a furnishing is a mark, so the hearth can be drawn"
```

---

### Task 11: Measure H1, then close

**Files:**
- Create: `clients/game/bin/examples/legend_redraw_bench.rs`
- Modify: `docs/timings.md`, `book/src/chronicle/the-legend.md`,
  `docs/retrospectives/the-legend.md`, `book/src/frontier/idea-registry.md`

- [ ] **Step 1: Measure the warm redraw**

200x200, coarsest rung, warm cache, on an IDLE box, replicated at least three
times. The Quadrat found the same measurement moved 1.40x on a loaded box and
refused to pick a fifth number; do not repeat that.

- [ ] **Step 2: Judge against the frozen bar**

Success criterion 1 (spec §7.1): **under 0.20 ms**, against The Quadrat's
0.056 ms baseline.

- Under 0.20 ms -> H1 SUPPORTED. Record the figure and the replicate spread.
- Over -> H1 FALSIFIED. **A result, not a failure** (decision 0016). Apply
  the designed response: `plate.rs` keeps a copy of the classifier GENERATED
  from the same source of truth rather than hand-written, so the two still
  cannot disagree. Report the number and the mechanism.
- Replicates disagree by more than 1.4x -> the box was not idle. Re-measure;
  do not average across a contended run.

- [ ] **Step 3: Ledger the run**

Append to `docs/timings.md` through the project's own recording path; do not
hand-edit a row.

- [ ] **Step 4: Update the registry rows this campaign moved**

`CLIENT-four-channels` (ratified in Task 2); `CLIENT-illumination-deferred`
(the client half is now closed); `CLIENT-glyph-carries-biome` (the capacity
observation is answered — by an ordinal ladder plus colour, NOT by the 22
glyphs it proposed; **say so explicitly** so the next reader does not think
the rejected design shipped).

Re-score `docs/audits/system-coverage-wolverson-2021.md` item **2.1** by
REGENERATING it. Never hand-edit a generated file.

- [ ] **Step 5: Chronicle and retrospective**

The chronicle carries findings at the book's altitude: technical,
mathematical, comprehensible without the code. Lead with the register and
the three collisions — that is the thing a reader will not guess.

The retrospective carries PROCESS lessons, and one is already known and must
be in it: the spec's §3 asserted the scene wire "fits a rung ladder" on the
strength of a verified NEIGHBOURING claim, and it was caught while writing
this plan rather than by the spec review.

- [ ] **Step 6: Full regeneration and gate**

```bash
make rebaseline
git diff --exit-code -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$')
make gate-commit
```

- [ ] **Step 7: Stage gate, then STOP at G6**

```bash
make sluice-stage BRANCH=campaign/the-legend REF=$(git rev-parse HEAD)
```

Then STOP. G6 is a hard stop: present the post-G3 ledger digest to Nathan and
wait. `closing-a-campaign` runs after he approves, unchanged.
