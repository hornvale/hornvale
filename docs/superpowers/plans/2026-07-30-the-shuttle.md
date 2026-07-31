# The Shuttle Implementation Plan

> **COMPLETE (2026-07-31).** All tasks executed (Task 6b added under ledger
> #7; Task 4 grew `parse_context_from` from Task 3's review). Measured:
> tongue 147→9.5 s, coherence 279→23 s, stitch 162→64 s, health
> 91.6→81.2 s; byte-identity proven by cross-binary cmp at every stage.
> See the chronicle and retrospective for the two falsified predictions
> and the one recorded spec deviation.

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Stop the chorus/diachronic readouts from re-sculpting the terrain on
every call by threading already-built terrain/climate through them (Stage 1),
and hoist `coexist::cell_share`'s per-cell allocations into dense
precomputed structures (Stage 2) — cutting the slowest gate tests 3–10×
with byte-identical output.

**Architecture:** Extend the existing `_from` artifact-threading idiom
(`lexicon_from`, `accounts_from`, `account_params_from` already exist) up
through the book's three entry points, the vessel `Session`, and the CLI.
Every existing `(world)`-shaped function keeps its signature and becomes a
delegating wrapper that builds the artifacts once. No caches, no new draws,
no save-format change.

**Tech Stack:** Rust edition 2024, workspace deps `serde`/`serde_json`/`libm`
only. Tests via `cargo nextest` / `cargo test`.

## Global Constraints

- **Byte-identity is the acceptance bar** (spec §3): every task's output
  must leave rendered volumes, ledgers, and committed artifacts
  byte-identical. Task 1 stages the reference binaries/outputs; Task 8
  verifies against them.
- **This campaign runs on lefford** (ledger #3): before ANY full `make gate`,
  run `bash scripts/census-run.sh status` and yield/wait if a heavy run
  holds the box. Iterate with `make quick` + scoped `cargo test -p <crate>`;
  the full gate runs at task boundaries, not per step.
- **Absorb main between tasks** (Nathan's standing rule): at each task
  boundary run `git fetch origin && git merge origin/main` in the worktree;
  on conflict, resolve and re-run the scoped tests before proceeding.
- No `HashMap`/`HashSet` (clippy enforces); dense `0..N`-keyed data uses
  `Vec` (kernel/CLAUDE.md); no wall-clock; `cargo fmt` before every commit.
- Every new `pub` item gets a one-line doc comment and, where it exposes a
  primitive, a `type-audit:` tag (`bare-ok(identifier-text: species)` is the
  house pattern on species/kind params — copy the tag style from the
  function you're twinning).
- Commit messages: `perf(the-shuttle): <what>` with body explaining why;
  every commit compiles and passes the tests it touches.
- The worktree is `/home/nathan/Projects/hornvale/.claude/worktrees/the-shuttle`
  (branch `the-shuttle`). All work happens there.
- **Do not regenerate the census.** Nothing in this plan licenses
  `HV_CENSUS=1` or `census-run.sh` (ledger #2). If a census fixture reds, a
  task has broken byte-identity — stop and investigate (systematic-debugging),
  never re-pin.

---

### Task 1: Stage the byte-identity reference and timing baseline

**Files:**
- Create: `/tmp/claude-1000/-home-nathan-Projects-hornvale/0f0bb752-022e-4d73-94df-65d89975bc7b/scratchpad/shuttle-reference/` (session scratch — transient, never committed)

**Interfaces:**
- Produces: `shuttle-reference/world-42.json`, `shuttle-reference/volume-1.txt`,
  `volume-2.txt`, `volume-3.txt`, `reckoning-1.txt`, and a timing record
  `timings-before.txt`, consumed by Task 8.

- [ ] **Step 1: Build the reference CLI binary at the pre-change tip and emit the reference outputs**

```bash
cd /home/nathan/Projects/hornvale/.claude/worktrees/the-shuttle
REF=/tmp/claude-1000/-home-nathan-Projects-hornvale/0f0bb752-022e-4d73-94df-65d89975bc7b/scratchpad/shuttle-reference
mkdir -p "$REF"
git log --oneline -1 | tee "$REF/base-commit.txt"   # must be the spec commit or a merge of origin/main
cargo run -q -p hornvale -- new --seed 42 --out "$REF/world-42.json"
cargo run -q -p hornvale -- book               > "$REF/book.txt"          2> "$REF/book.stderr.txt"
cargo run -q -p hornvale -- book --initiate    > "$REF/book-initiate.txt" 2>/dev/null
cargo run -q -p hornvale -- book --at 36525    > "$REF/book-at.txt"       2>/dev/null
```

(Verified against `cli/src/main.rs`: `book` takes no seed flag — one run
renders all three volumes for seeds 1–3, and `--initiate`/`--at <DAY>` are
its two lenses, main.rs:81–84. The stderr capture keeps the PROC-15
coverage report for comparison too.)

- [ ] **Step 2: Record the before-timings of the four target tests**

Run each ONCE, on a quiet box (check `uptime`; loadavg should be < 4):

```bash
cd /home/nathan/Projects/hornvale/.claude/worktrees/the-shuttle
cargo test --no-run -p hornvale-book -p hornvale-worldgen -p hornvale-vessel -p hornvale-lab 2>&1 | tail -1
{ time cargo test -p hornvale-book tongue_lines_are_deterministic -- --exact tests::tongue_lines_are_deterministic ; } 2>&1 | tail -5 | tee -a "$REF/timings-before.txt"
{ time cargo test -p hornvale-worldgen --test deep_grammar the_coherence_law -- --exact ; } 2>&1 | tail -5 | tee -a "$REF/timings-before.txt"
{ time cargo test -p hornvale-vessel --test session the_stitch_law_end_to_end -- --exact ; } 2>&1 | tail -5 | tee -a "$REF/timings-before.txt"
{ time cargo test -p hornvale-lab --test health_calibration the_null_control_reads_no_chronic_distress -- --exact ; } 2>&1 | tail -5 | tee -a "$REF/timings-before.txt"
```

(The committed lefford baseline says ~250 s / ~309 s / ~250 s / ~168 s; your
numbers should be within ~20 % of those — if wildly off, the box is loaded,
re-run.)

- [ ] **Step 3: Commit nothing** — this task produces scratch state only.
Verify `git status --porcelain` in the worktree is clean.

---

### Task 2: Worldgen — complete and publish the `_from` readout family

**Files:**
- Modify: `windows/worldgen/src/chorus.rs` (readouts at lines cited below)
- Test: `windows/worldgen/tests/threading_equivalence.rs` (new)

**Interfaces:**
- Consumes: existing `pub(crate) fn account_params_from(world, species, &GeneratedTerrain, &GeneratedClimate) -> Result<AccountParams, BuildError>` (chorus.rs:1204), `pub(crate) fn cyclic_beliefs_from(world, species, &GeneratedClimate) -> Vec<(Belief, f64)>` (chorus.rs:~225), `pub fn accounts_from(world, &GeneratedTerrain, &GeneratedClimate)` (chorus.rs:1413), `pub fn terrain_of(world)` (lib.rs:458), `pub fn climate_from(world, &terrain)` (lib.rs:1470).
- Produces (all `pub`, all in `chorus.rs`, loose-parts signature matching the existing `_from` convention):
  - `pub fn account_params_from(...)` — same fn, visibility raised from `pub(crate)`
  - `pub fn cyclic_beliefs_from(...)` — same, visibility raised
  - `pub fn doctrine_from(world: &World, species: &str, terrain: &hornvale_terrain::GeneratedTerrain, climate: &hornvale_climate::GeneratedClimate) -> Option<DoctrineVoice>`
  - `pub fn doctrines_from(world, terrain, climate) -> Vec<DoctrineVoice>`
  - `pub fn day_schema_from(world, species, terrain, climate) -> Option<SchemaId>`
  - `pub fn noun_class_from(world, species, concept: &str, terrain, climate) -> NounClass`
  - `pub fn observations_from(world, species, at: StdDays, terrain, climate) -> Result<Observations, BuildError>`
  - `pub fn ladder_from(world, species, at: StdDays, terrain, climate) -> Result<(LadderRung, Option<f64>), BuildError>`

Every existing `_of` form becomes a thin wrapper: sculpt once
(`let terrain = crate::terrain_of(world)?; let climate = crate::climate_from(world, &terrain)?;`)
then delegate — exactly the shape `account_params_of` (chorus.rs:1191–1195)
and `accounts_of` (chorus.rs:1399–1408) already have. NOTE `chorus_ground`
(chorus.rs:1332) does NOT sculpt — pure ledger reads — leave it untouched.
`tongue_morphology_of` (chorus.rs:1457) does not sculpt either (wc + draws
via `cascade_of`, lib.rs:4425) — leave it untouched.

- [ ] **Step 1: Write the failing equivalence test**

Create `windows/worldgen/tests/threading_equivalence.rs`. Ground: the build
helper shape is the same as `windows/book/src/lib.rs` `generated()`
(~line 2676); `placed_peoples` is `hornvale_worldgen::placed_peoples(world)`.
ONE test, ONE build — this binary must stay cheap:

```rust
//! The Shuttle's equivalence pins: every `_from` readout equals its
//! `_of` wrapper on the same world. One world, one sculpt, all pairs.

use hornvale_astronomy::SkyPins;
use hornvale_kernel::Seed;
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{SettlementPins, SkyChoice, build_world};

#[test]
fn from_variants_equal_their_of_wrappers() {
    let world = build_world(
        Seed(1),
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
    )
    .expect("seed 1 builds");
    let terrain = hornvale_worldgen::terrain_of(&world).expect("terrain reconstructs");
    let climate = hornvale_worldgen::climate_from(&world, &terrain).expect("climate derives");
    let at = hornvale_astronomy::StdDays::new(36_525.0).expect("valid day");

    for (kind, _v) in hornvale_worldgen::placed_peoples(&world) {
        assert_eq!(
            format!("{:?}", hornvale_worldgen::account_params_of(&world, kind).ok()),
            format!("{:?}", hornvale_worldgen::account_params_from(&world, kind, &terrain, &climate).ok()),
            "account_params diverged for {kind}"
        );
        assert_eq!(
            format!("{:?}", hornvale_worldgen::cyclic_beliefs_of(&world, kind)),
            format!("{:?}", hornvale_worldgen::cyclic_beliefs_from(&world, kind, &climate)),
            "cyclic_beliefs diverged for {kind}"
        );
        assert_eq!(
            format!("{:?}", hornvale_worldgen::doctrine_of(&world, kind)),
            format!("{:?}", hornvale_worldgen::doctrine_from(&world, kind, &terrain, &climate)),
            "doctrine diverged for {kind}"
        );
        assert_eq!(
            hornvale_worldgen::day_schema_of(&world, kind),
            hornvale_worldgen::day_schema_from(&world, kind, &terrain, &climate),
            "day_schema diverged for {kind}"
        );
        for concept in ["sun", "moon", "star", "earth", "person", "river"] {
            assert_eq!(
                hornvale_worldgen::noun_class_of(&world, kind, concept),
                hornvale_worldgen::noun_class_from(&world, kind, concept, &terrain, &climate),
                "noun_class diverged for {kind}/{concept}"
            );
        }
        assert_eq!(
            format!("{:?}", hornvale_worldgen::observations_of(&world, kind, at).ok()),
            format!("{:?}", hornvale_worldgen::observations_from(&world, kind, at, &terrain, &climate).ok()),
            "observations diverged for {kind}"
        );
        assert_eq!(
            format!("{:?}", hornvale_worldgen::ladder_of(&world, kind, at).ok()),
            format!("{:?}", hornvale_worldgen::ladder_from(&world, kind, at, &terrain, &climate).ok()),
            "ladder diverged for {kind}"
        );
    }
}
```

(`format!("{:?}", …)` sidesteps missing `PartialEq` on the report types —
`AccountParams`/`DoctrineVoice` derive `Debug` but not `Eq`. If a type
turns out to lack `Debug`, derive nothing new; compare the fields the book
actually consumes instead, and say so in the commit body.)

- [ ] **Step 2: Run it to verify it fails to COMPILE** (the `_from` names
don't exist / aren't pub yet):

```bash
cargo test -p hornvale-worldgen --test threading_equivalence 2>&1 | tail -5
```
Expected: compile error naming `doctrine_from` etc.

- [ ] **Step 3: Implement the family in `chorus.rs`**

Mechanics per function (all bodies already exist — this is re-plumbing):
- `account_params_from`, `cyclic_beliefs_from`: change `pub(crate) fn` →
  `pub fn`; extend each doc comment's first line to note it is the threaded
  twin (keep the existing byte-identity sentences).
- `doctrine_from`: copy `doctrine_of`'s body (chorus.rs:1131–1149); replace
  `account_params_of(world, species)` with
  `account_params_from(world, species, terrain, climate)`. `doctrine_of`
  becomes: sculpt once, delegate. `doctrines_of` (chorus.rs:1158) keeps its
  roster loop but sculpts ONCE above the loop and calls `doctrine_from`;
  add `doctrines_from` with the loop body.
- `day_schema_from`: copy `day_schema_of`'s body (chorus.rs:1502–1535);
  replace the `account_params_of` call with `account_params_from`. The rest
  of the body (`chorus_ground`, `account_of`, `flagship_of`,
  `subsistence_of`, `WorldComponents::assemble`) does not sculpt — leave it.
- `noun_class_from`: copy `noun_class_of`'s body (chorus.rs:1546–1560);
  the only change is the SKY_OVERRIDE arm calling `day_schema_from`.
- `observations_from`: copy `observations_of`'s body (chorus.rs:1620–1646);
  swap `account_params_of` → `account_params_from`.
- `ladder_from`: copy `ladder_of`'s body (chorus.rs:1780+); swap
  `observations_of` → `observations_from` and `doctrine_of` → `doctrine_from`.

Every wrapper keeps its exact current signature and doc comment (append one
sentence pointing at the `_from` twin).

**Re-export:** add every new/promoted `_from` name to the crate-root
re-export list `pub use chorus::{ … }` (windows/worldgen/src/lib.rs:92) —
book and vessel call these as `hornvale_worldgen::doctrine_from` etc., the
same way they already call `hornvale_worldgen::doctrine_of`.

- [ ] **Step 4: Run the equivalence test and the crate's scoped tests**

```bash
cargo test -p hornvale-worldgen --test threading_equivalence 2>&1 | tail -3
cargo test -p hornvale-worldgen --test chorus_params 2>&1 | tail -3
cargo test -p hornvale-worldgen --test diachronic 2>&1 | tail -3
cargo test -p hornvale-worldgen --test doctrine 2>&1 | tail -3
```
Expected: PASS (the diachronic/doctrine batteries are slow — run once, read
the exit code; do NOT re-run to grep).

- [ ] **Step 5: fmt, clippy, type-audit, commit**

```bash
make quick 2>&1 | tail -3
git add -A && git commit -m "perf(the-shuttle): publish the _from readout family in chorus

account_params_from/cyclic_beliefs_from go pub; doctrine, day_schema,
noun_class, observations, ladder gain _from twins threading built
terrain/climate. Wrappers keep every existing signature and sculpt once.
Equivalence pinned by tests/threading_equivalence.rs (one world, all pairs)."
```

---

### Task 3: Book — thread the bundle through all three entry points

**Files:**
- Modify: `windows/book/src/lib.rs` (`render_volume` :267, `chorus_sections`
  :485, `reckoning_epochs` :589, `reckoning_at` :628, `reckoning_epoch`
  :692, tongue section :357–445, `esoteric_lines` :1570)

**Interfaces:**
- Consumes: Task 2's `_from` family; `hornvale_worldgen::{terrain_of, climate_from, lexicon_from}` (lexicon_from at worldgen lib.rs:4529, signature `(world, species, &terrain, &climate)`).
- Produces (pub, in `hornvale_book`):
  - `pub fn render_volume_from(world: &World, terrain: &hornvale_terrain::GeneratedTerrain, climate: &hornvale_climate::GeneratedClimate) -> BookVolume`
  - `pub fn reckoning_at_from(world, at: StdDays, terrain, climate) -> ReckoningEpoch`
  - `pub fn esoteric_lines_from(world, reader: &BTreeSet<(String, String)>, terrain, climate) -> Vec<String>`
  - The `(world)` forms remain, each sculpting once and delegating.

- [ ] **Step 1: Add an in-module equivalence test (write it failing first)**

In `windows/book/src/lib.rs` `mod tests`, next to
`tongue_lines_are_deterministic` (:3108):

```rust
/// The Shuttle: the threaded entry points equal their wrappers — one
/// world, one sculpt, byte-equal volumes.
#[test]
fn from_entry_points_equal_their_wrappers() {
    let world = generated(1);
    let terrain = hornvale_worldgen::terrain_of(&world).expect("terrain reconstructs");
    let climate = hornvale_worldgen::climate_from(&world, &terrain).expect("climate derives");
    let a = render_volume(&world);
    let b = render_volume_from(&world, &terrain, &climate);
    assert_eq!(a.lines, b.lines);
    assert_eq!(a.tongue_lines, b.tongue_lines);
    assert_eq!(a.tongue_gaps, b.tongue_gaps);
    assert_eq!(
        format!("{:?}", a.reckoning),
        format!("{:?}", b.reckoning)
    );
}
```

- [ ] **Step 2: Verify it fails to compile** (`render_volume_from` absent):
`cargo test -p hornvale-book from_entry_points 2>&1 | tail -3`

- [ ] **Step 3: Implement the threading**

- `render_volume_from(world, terrain, climate)` gets `render_volume`'s
  body. Inside it: `chorus_sections`/`reckoning_epochs` become
  `chorus_sections_from`/`reckoning_epochs_from` (private, threaded);
  `hornvale_worldgen::accounts_of(world)` → `accounts_from(world, terrain, climate)`;
  `doctrine_of` → `doctrine_from`; `lexicon_of` (:373, :497) →
  `lexicon_from(world, kind, terrain, climate)`;
  `observations_of`/`ladder_of` in `reckoning_epoch` (:716, :723) → `_from`.
- **The `noun_class_of` closures** (:380 area and :511 area): compute the
  sky answer ONCE per kind —
  ```rust
  let sky_animate = hornvale_worldgen::day_schema_from(world, kind, terrain, climate)
      == Some(hornvale_language::SchemaId::Agentive);
  // (SchemaId is hornvale_language's, domains/language/src/schemas.rs:101,
  // re-exported through worldgen's chorus — book already depends on
  // hornvale-language directly, so import whichever path lib.rs already
  // uses for NounClass and stay consistent.)
  let noun_class_of = |concept: &str| {
      const SKY_OVERRIDE: [&str; 4] = ["sun", "moon", "star", "earth"];
      if SKY_OVERRIDE.contains(&concept) {
          if sky_animate { hornvale_language::NounClass::Animate } else { hornvale_language::NounClass::Inanimate }
      } else if concept == "person" || concept.ends_with("-kind") {
          hornvale_language::NounClass::Animate
      } else {
          hornvale_language::NounClass::Inanimate
      }
  };
  ```
  This duplicates `noun_class_of`'s non-sky arm (chorus.rs:1546–1560). If
  that feels like a drifting copy, the alternative is
  `hornvale_worldgen::noun_class_from` per concept — correct but re-runs
  `day_schema_from`'s non-sculpt tail per concept; EITHER is acceptable,
  but if you copy, add `// mirror of chorus::noun_class_of — the sky arm is
  hoisted; keep the non-sky arms in lockstep` and prefer exposing a
  `pub fn noun_class_with_sky(sky_animate: bool, concept: &str) -> NounClass`
  helper in chorus.rs that BOTH paths call, so there is one copy. Do the
  helper version unless it snowballs.
- `render_volume(world)` becomes: sculpt once
  (`terrain_of` + `climate_from`, `.expect()` is NOT the current posture —
  `render_volume` today silently skips on failed readouts; match the
  existing posture: on sculpt failure return the volume built from an
  empty-account path by calling the readout wrappers as before. Simplest
  faithful shape: keep the wrapper calling `render_volume_from` only when
  both artifacts build, else fall back to the old un-threaded body? NO —
  duplicate body is exactly what we're removing. Instead: `terrain_of`
  fails only on malformed pin facts (lib.rs:458–469), which a built world
  cannot have; `accounts_of` (chorus.rs:1399) already silently returns
  empty on that same failure. Mirror THAT: on sculpt failure return
  `BookVolume { seed: world.seed.0, lines: Vec::new(), … }` — and note in
  the doc comment that a world whose pins fail to parse renders an empty
  volume, same as `accounts_of`'s posture.)
- Same pattern for `reckoning_at`/`reckoning_at_from` (:628) and
  `esoteric_lines`/`esoteric_lines_from` (:1570; its loop's
  `accounts_of` + per-voice `doctrine_of` both switch to `_from`).

- [ ] **Step 4: Run the book's fast tests + the equivalence test**

```bash
cargo test -p hornvale-book from_entry_points 2>&1 | tail -3
cargo test -p hornvale-book coverage_flags_name_as_uncovered 2>&1 | tail -3
cargo test -p hornvale-book tongue_lines_are_deterministic 2>&1 | tee /tmp/hv-shuttle-t3.txt | tail -3
```
Expected: all PASS, and the tongue test's wall time (visible in the output)
collapses from ~250 s to well under 30 s. Record the number in the ledger.

- [ ] **Step 5: fmt/quick, commit**

```bash
make quick 2>&1 | tail -3
git add -A && git commit -m "perf(the-shuttle): book renders sculpt once per entry point

render_volume/reckoning_at/esoteric_lines gain _from twins; internals
thread terrain+climate through chorus_sections, reckoning_epochs, the
tongue section, and the noun-class closure (day-schema computed once per
kind via chorus's shared noun_class_with_sky helper). Wrappers keep their
signatures; a world whose pins fail to parse renders empty, matching
accounts_of's existing posture."
```

---

### Task 4: Vessel — the Session sculpts once at start

**Files:**
- Modify: `windows/vessel/src/session.rs` (struct :183, `start` :295,
  `consult` :2070–2090)

**Interfaces:**
- Consumes: Task 3's `reckoning_at_from`/`esoteric_lines_from`; worldgen's
  `demography_report_from` (lib.rs:1409) and any `_from` pressure-field
  variants that exist (grep `prey_pressure`/`predator_pressure`/
  `wild_concentrations` for `_from`/`_in` twins before assuming).
- Produces: `Session` gains two private fields —
  `terrain: Option<hornvale_terrain::GeneratedTerrain>`,
  `climate: Option<hornvale_climate::GeneratedClimate>` — built once in
  `start`, `None` exactly when reconstruction fails (matching `calendar`'s
  and `predator`'s existing `Option` posture at :202–:208).

- [ ] **Step 1: Read `Session::start` end-to-end and inventory its sculpts.**
Grep the body for `terrain_of`, `climate_of`, `demography_report_of`,
`prey_pressure`, `predator_pressure`, `wild_concentrations`,
`LocaleContext::build`. For each hit note whether a `_from` twin exists in
worldgen (grep `pub fn <name>_from\|pub fn <name>_in`). Add the inventory
to the task's commit body.

- [ ] **Step 2: Implement.** In `start`: build `terrain`/`climate` once
right after the world is accepted; pass them to every call site from the
Step-1 inventory that has a `_from` twin; store them on the struct. A call
site with NO twin is left alone and listed in the commit body (it becomes
followup material, NOT an inline refactor — do not add new worldgen
functions in this task; Task 2 owns the worldgen surface).
In `consult` (:2082, :2085):

```rust
let epoch = match (self.terrain.as_ref(), self.climate.as_ref()) {
    (Some(t), Some(c)) => hornvale_book::reckoning_at_from(self.world, at, t, c),
    _ => hornvale_book::reckoning_at(self.world, at),
};
// … and the same split for esoteric_lines_from(self.world, &reader_set(&self.knowledge), t, c)
```

- [ ] **Step 3: Run the vessel's scoped tests**

```bash
cargo test -p hornvale-vessel --test session 2>&1 | tee /tmp/hv-shuttle-t4.txt | tail -5
cargo test -p hornvale-vessel --test the_purview 2>&1 | tail -3
```
Expected: PASS; `the_stitch_law_end_to_end`'s time drops (record it).

- [ ] **Step 4: fmt/quick, commit** (message pattern as before; include the
Step-1 inventory and which sites remain unthreaded and why).

---

### Task 5: CLI — the command surface sculpts once

**Files:**
- Modify: `cli/src/main.rs` (:869 `render_volume`, :937 `reckoning_at`,
  :969 `esoteric_lines`)

**Interfaces:**
- Consumes: Task 3's `_from` entry points.

- [ ] **Step 1: Thread `cmd_book` (main.rs:855).** Its per-seed loop BUILDS
each world (main.rs:860–868) then calls `render_volume(&world)` (:869), and
the `--at`/`--initiate` lenses call `reckoning_at` (:937) and
`esoteric_lines` (:969) on the same worlds. Inside the loop, right after
the build, add once per seed:
`let terrain = hornvale_worldgen::terrain_of(&world);` +
`let climate = terrain.as_ref().ok().and_then(|t| hornvale_worldgen::climate_from(&world, t).ok());`
and switch each of the three calls to its `_from` form when both artifacts
are available, else the wrapper (same fallback split as Task 4's
`consult`). (Sharper alternative if the loop's build call makes it easy:
`build_world_to_with_artifacts` (worldgen lib.rs:5035) already returns
`BuildArtifacts { world, terrain, climate }` — reusing the BUILD's own
artifacts instead of reconstructing them saves one more sculpt per seed;
prefer it if the surrounding code stays readable.)

- [ ] **Step 2: Verify the rendered output is byte-identical**

```bash
REF=/tmp/claude-1000/-home-nathan-Projects-hornvale/0f0bb752-022e-4d73-94df-65d89975bc7b/scratchpad/shuttle-reference
cargo run -q -p hornvale -- book > /tmp/hv-shuttle-book.txt 2>/dev/null
diff /tmp/hv-shuttle-book.txt "$REF/book.txt" && echo IDENTICAL
cargo run -q -p hornvale -- book --initiate 2>/dev/null | diff - "$REF/book-initiate.txt" && echo INITIATE-IDENTICAL
cargo run -q -p hornvale -- book --at 36525 2>/dev/null | diff - "$REF/book-at.txt" && echo AT-IDENTICAL
```
Expected: IDENTICAL ×3. A diff here is a defect — stop,
systematic-debugging.

- [ ] **Step 3: Run the CLI crate tests, fmt/quick, commit**

```bash
cargo test -p hornvale 2>&1 | tail -3
make quick 2>&1 | tail -3
git add -A && git commit -m "perf(the-shuttle): CLI book/reckoning/initiate render through the threaded entry points"
```

---

### Task 6: Stage 1 verification — byte-identity and the measured claim

**Files:** none created in-repo (ledger + scratch only)

- [ ] **Step 1: Absorb main** (`git fetch origin && git merge origin/main`),
re-run `make quick`.

- [ ] **Step 2: Full byte-identity sweep against the Task 1 reference**

```bash
cd /home/nathan/Projects/hornvale/.claude/worktrees/the-shuttle
REF=/tmp/claude-1000/-home-nathan-Projects-hornvale/0f0bb752-022e-4d73-94df-65d89975bc7b/scratchpad/shuttle-reference
cargo run -q -p hornvale -- new --seed 42 --out /tmp/hv-shuttle-world42.json
cmp /tmp/hv-shuttle-world42.json "$REF/world-42.json" && echo WORLD-IDENTICAL
cargo run -q -p hornvale -- book 2>/dev/null | diff - "$REF/book.txt" && echo BOOK-IDENTICAL
cargo run -q -p hornvale -- book --initiate 2>/dev/null | diff - "$REF/book-initiate.txt" && echo INITIATE-IDENTICAL
cargo run -q -p hornvale -- book --at 36525 2>/dev/null | diff - "$REF/book-at.txt" && echo AT-IDENTICAL
```
Expected: every line IDENTICAL. (The world JSON cannot differ — the build
path is untouched — but the Occlusion rule is to verify the total route,
not reason about it.)

- [ ] **Step 3: Artifact drift check**

```bash
make rebaseline 2>&1 | tail -3
git diff --exit-code book/src/gallery/ book/src/reference/ book/src/laboratory/ docs/audits/ && echo NO-DRIFT
git checkout -- . 2>/dev/null; git status --porcelain | head   # discard incidental regen noise ONLY if NO-DRIFT printed; if drift appeared, STOP and investigate
```

- [ ] **Step 4: Timing readout** — re-run the Task 1 Step 2 commands, tee
into `$REF/timings-after-stage1.txt`. Success criteria (spec §4):
tongue < 25 s, coherence < 30 s, stitch < 90 s. A miss is a FINDING —
flamegraph the miss (`perf record -F 99 --call-graph fp` on the frame-
pointer build, see the memory recipe), record the new dominant cost in the
ledger, do not retune.

- [ ] **Step 5: Full gate** (claim check first):

```bash
bash scripts/census-run.sh status   # must print "no heavy run in progress"
make gate 2>&1 | tail -5
```
Expected: green. Commit any stragglers; ledger the gate time.

---

### Task 7: Stage 2 — dense-index `coexist::pack`

**Files:**
- Modify: `domains/demography/src/coexist.rs` (`cell_share` :71, `pack` :357)
- Test: existing `domains/demography` tests + `windows/lab` health pins

**Interfaces:**
- Consumes: nothing from other tasks (independent, revertible).
- Produces: `pack`'s per-cell loop calls a new private
  `fn cell_share_indexed(capacity: f64, present: &[(u32, f64)], powered: &[f64], overlap_dense: &[Vec<f64>], idx_of: &BTreeMap<u32, usize>, beta: f64, floor_pow: f64) -> BTreeMap<u32, f64>`
  — pub `cell_share` keeps its exact signature and becomes a wrapper that
  builds the dense structures for its single call (so its unit tests and
  any external caller are untouched).

**The order constraint (spec §2, ledger #2):** the hoist must not change a
single float operation's order. `cell_share` today (coexist.rs:71–110):
sorts `present` by id, computes `floor_pow = powf(floor, beta)`, computes
`powered[id] = powf(k, beta)` in sorted order, then per species `s` computes
`weighted_sum = Σ_j weight(s,j) * powered[j]` in sorted-j order. The indexed
version must iterate identically: same sort (done ONCE in `pack`'s per-cell
loop, which already sorts at :396 — delete cell_share's defensive re-sort
ONLY on the indexed path), same `powered` values in the same order (a `Vec`
indexed by position replaces the `BTreeMap` keyed by id — position order IS
sorted-id order, so iteration is unchanged), same `weighted_sum` fold. The
dense overlap matrix `overlap_dense[i][j] = overlap.get(&(id_i, id_j)).copied().unwrap_or(0.0)`
is precomputed once per `pack` call (species set is cell-invariant, built
from the sorted species roster).

- [ ] **Step 1: Run the existing demography tests to green first** (they are
the pins): `cargo test -p hornvale-demography 2>&1 | tail -3`

- [ ] **Step 2: Implement** as described. In `pack`'s per-cell loop, map
each cell's `present` ids to roster positions via a `BTreeMap<u32, usize>`
built once (a cell's present set is a subset of the roster — positions
still ascend with id, preserving order).

- [ ] **Step 3: Prove the pins hold**

```bash
cargo test -p hornvale-demography 2>&1 | tail -3
cargo test -p hornvale-lab --test health_calibration the_null_control_reads_no_chronic_distress -- --exact 2>&1 | tee /tmp/hv-shuttle-t7.txt | tail -5
```
Expected: PASS, byte-identical pins, and the health test's time drops
(record before/after in the ledger; the spec predicts the `cell_share` flat
share ~13 % vanishes — the sim-loop share stays).

- [ ] **Step 4: fmt/quick, commit**

```bash
make quick 2>&1 | tail -3
git add -A && git commit -m "perf(the-shuttle): dense-index coexist::pack's per-cell loop

overlap goes to a position-indexed dense matrix built once per pack call;
powered becomes a per-cell Vec in sorted-id order; the indexed path skips
cell_share's defensive re-sort (pack already sorts at :396). Same floats,
same order — cell_share's pub signature and unit behavior unchanged
(wrapper builds the dense structures for its one call). The Lookup's
dense-index rule applied to the health battery's hottest flat frame."
```

---

### Task 8: Close — gate, absorb, evidence, G6 package

- [ ] **Step 1: Absorb main; re-run the byte-identity sweep** (Task 6 Steps
2–3 verbatim — an absorbed commit may have moved a golden; attribute any
drift to its mover before touching anything, per the inherited-debt memory).
- [ ] **Step 1b: Regenerate the committed type-audit report** — this
campaign adds pub-boundary items, so `docs/audits/type-audit-report.md` is
guaranteed stale, and NOTHING in the local gate says so (`make gate` runs
`check`, not `report`; CI is manual-only). AFTER the last absorb (a
pre-absorb regen reverts main's own report entries — the standing memory):
`cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md`
then commit the diff. (Task 2's review, finding 5.)
- [ ] **Step 2: Full gate on a quiet box** (claim check first), then
`make preflight` from the branch.
- [ ] **Step 3: Final timing table** — the four target tests plus a whole
`make ci`-equivalent scoped read: paste before/after into the ledger and
into the chronicle draft. Do NOT hand-edit
`docs/timings/test-baseline-lefford.tsv` — the alarm re-records itself on
the next green `make ci` (verified slower-only, timings.rs:350/:401).
- [ ] **Step 4: Write the chronicle entry** (`book/src/chronicle/`), the
retrospective (`docs/retrospectives/the-shuttle.md`, process lessons only),
promote `.superpowers/sdd/followups.md` into the retro, and sweep stale
book chapters (the laboratory/perf-adjacent pages; the Confidence Gradient
is unaffected — no bets moved).
- [ ] **Step 5: Assemble the G6 package** (post-G3 ledger digest,
determinism entries leading) and STOP for Nathan. Merge and teardown happen
only after his sign-off, via the closing-a-campaign skill, ff-merged from
`~/Projects/hornvale`, never from inside the worktree (the merge memory).
