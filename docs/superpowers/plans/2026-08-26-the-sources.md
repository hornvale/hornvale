# The Sources Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Derive a per-rung subterranean energy field from lithology, make
`TrophicMode::Chemotrophic` witnessed, and resolve the underworld's three
occupied environment axes per rung instead of once per column.

**Architecture:** Six named energy sources, each a pure function of shipped
lithology and the geothermal gradient, summed onto the kernel's existing
`ENERGY` ruler. The underworld's `ENERGY`/`WATER`/`SUBSTRATE` axes move from
one evaluation per vertex (at the cave's `depth_reach_m`) to one per rung (at
the rung's ΔT midpoint), which is the resolution at which the shipped claim
that energy *inverts* with depth is expressible at all. A new
`CHEMOSYNTHATE` resource axis carries that energy to consumers, and `xorn` —
already `Absent`/`Absent` with a pure-`MINERAL` niche and an authored comment
saying it burrows through stone — becomes the variant's witness.

**Tech Stack:** Rust 2024, `hornvale-kernel` / `hornvale-terrain` /
`hornvale-climate` / `hornvale-species` / `hornvale-worldgen`. No new
dependencies (decision 0004: `serde`, `serde_json`, `libm` only).

**Spec:** `docs/superpowers/specs/2026-08-26-the-sources-design.md`

## Global Constraints

- **No new dependencies.** The allowlist is `ALLOWED_EXTERNAL` in
  `cli/tests/architecture.rs`.
- **No `HashMap`/`HashSet`** — `BTreeMap`/`BTreeSet`/`Vec` only, enforced by
  `clippy.toml`. Float sorting uses `total_cmp`.
- **No wall-clock time.** Time is `WorldTime { ticks: i64 }`.
- **Quantize at emit only.** Never in the compute path.
- **Layering:** `kernel/` → `domains/*` → `windows/*` → `cli/`. A domain
  crate depends on `hornvale-kernel` and no sibling domain.
- **Every crate sets `#![warn(missing_docs)]`.** Every public item, field
  and variant gets a one-line doc comment.
- **Every primitive at a `pub` boundary carries a `type-audit:` verdict
  tag** (`bare-ok(<class>)` / `waiver(<reason>)` / `pending(wave-N)`).
- **`cargo fmt` is the final step before every commit.** Fmt-gate skips are
  the single most common review finding in this project.
- **`make gate-commit` before every commit.** It is local and seconds-scale.

## Rulings that bind every task

### Ruling 1 — a test that has never failed is not a test

Every task that adds an assertion carries a **mutation control**: change the
production code so the new assertion *should* fail, run it, paste the failure
message into the task report, restore. A green suite is not evidence the
assertion can fire.

**This is not optional here and the reason is on the record.**
`windows/worldgen/tests/suite/underworld_conditions_probe.rs:129` states that
neutralising the depth `subterranean_substrate_field` derives left **all 614
worldgen tests green**. The area this campaign works in is known to be
weakly pinned.

Use `scripts/mutate.py` rather than `sed`: it substitutes only if the target
text is found and unique, so a `cargo fmt` rewrap cannot silently turn your
mutation into a no-op. **Never use `git checkout` to restore mid-mutation** —
use `mutate.py --to`. And restoring the source is not enough on its own:
confirm the *restored* build is what runs, because cargo can serve a stale
binary from the mutated build and the silent direction is a false GREEN.

### Ruling 2 — do not prescribe your own mutation from this document

Where a task says "demonstrate property P by mutation", **find the mutation
yourself by reading the code**. This plan deliberately does not name specific
edits. A plan author does not know which derivations share a stream or which
constants are load-bearing; the implementer does, after reading. Both
prescribed mutations in The Quire were nulls and the implementer found a
discriminating one by hunting, in both cases.

### Ruling 3 — you may override this plan, and must say so

If the tree disagrees with a task's brief, **the tree wins**. Report the
disagreement in your task report rather than making the code match the plan.
Every defect in The Gossan was in plan or spec prose and none was in
implementer code; the implementers who flagged rather than complied are why.

### Ruling 4 — never rename a test

`docs/timings/subfloor-roster.tsv` selects tests by **exact name**. A rename
drops the test from `make gate-commit` until a green chamber run rewrites the
roster. Where a task changes what a test asserts, change its *body* and leave
its *name* alone unless the task explicitly says otherwise and states the
consequence.

### Ruling 5 — a new test does not run under `make gate-commit`

Until a green chamber run records its baseline duration in the roster, a new
test is invisible to the commit gate. **Run every new test explicitly, by
name**, and paste the output. A green `make gate-commit` says nothing about a
test written in that same commit.

### Ruling 6 — regenerate aggregates, never hand-resolve them

On any merge conflict in `docs/audits/type-audit-report.md`,
`docs/digest/decisions-in-force.md`, or `docs/digest/intent-vs-reality.md`:
regenerate with the command in `CLAUDE.md`, never text-merge. A text
resolution ships an aggregate that is a valid merge of neither side.
`book/src/frontier/idea-registry.md` is hand-edited and so the
never-text-merge habit does *not* fire for it — check
`registry_ids_are_unique` after any absorption.

### Ruling 7 — this campaign moves numbers, and attribution is the point

Tasks 1-8 are additive or behaviour-preserving. **Task 9 is the only task
that moves a shipped world's numbers.** Keep it that way: if an earlier task
moves a golden, stop and report it rather than rebaselining, because it means
something was not as additive as this plan believed.

## File map

```
kernel/src/ecology.rs                      CHEMOSYNTHATE axis (Task 7)
domains/terrain/src/delve.rs               rung evaluation depth rule (Task 2)
domains/species/src/lib.rs                 xorn's trophic mode + niche (Task 8)
domains/species/tests/suite/metabolic_pairs.rs
                                           the two handoff tests (Task 8)
windows/worldgen/src/energy.rs             NEW -- the six sources (Tasks 4, 5)
windows/worldgen/src/lib.rs                per-rung field, the switch (Tasks 3, 9, 10)
windows/worldgen/tests/suite/              harvested probes (Task 1),
                                           the U test (Task 5),
                                           between-worlds (Task 6)
book/src/frontier/idea-registry.md         3 harvested rows (Task 1),
                                           MAP-per-rung-substrate (Task 9)
docs/superpowers/specs/...-metaplan.md     re-run figures (Task 1)
```

`windows/worldgen/src/energy.rs` is a new module rather than more of
`lib.rs`, which is already ~15,000 lines. Six source functions, their sum,
and the dominant-source projection are one responsibility and belong
together.

---

### Task 1: Harvest The Winze's measurement stratum, and re-run it

**Files:**
- Create (cherry-picked): `windows/worldgen/tests/suite/winze_scale_probe.rs`,
  `winze_energy_probe.rs`, `ore_separation_probe.rs`,
  `ore_viability_probe.rs`, `off_lithology_decorrelation_probe.rs`
- Modify: `windows/worldgen/tests/suite.rs` (module declarations)
- Modify: `book/src/frontier/idea-registry.md` (three rows)
- Modify: `docs/superpowers/specs/2026-08-24-the-underworld-larder-metaplan.md`

**Interfaces:**
- Consumes: nothing.
- Produces: the measured figures every later task's design rests on, and the
  registry row `BIO-subterranean-energy-sources`.

**Why this is first.** `main` cites
`windows/worldgen/tests/suite/winze_scale_probe.rs` at metaplan line 39 for
"chambers 3.264x" and that file is not on main, while
`book/src/frontier/idea-registry.md:622` still carries the size clause the
same metaplan calls falsified. Nothing mechanical objects to either.

- [ ] **Step 1: Read the branch before touching it**

```bash
git log --oneline main..campaign/the-winze
git diff --stat $(git merge-base main campaign/the-winze) campaign/the-winze
```

Expected: 25 commits, 10 files, zero production code. **If you find
production code, STOP and report** — this task's premise is that the
measurement stratum is separable, and production code means it is not.

- [ ] **Step 2: Bring the five probe files and the suite declarations across**

```bash
for f in winze_scale_probe winze_energy_probe ore_separation_probe \
         ore_viability_probe off_lithology_decorrelation_probe; do
  git show campaign/the-winze:windows/worldgen/tests/suite/$f.rs \
    > windows/worldgen/tests/suite/$f.rs
done
git diff $(git merge-base main campaign/the-winze) campaign/the-winze \
  -- windows/worldgen/tests/suite.rs
```

Add the declarations to `windows/worldgen/tests/suite.rs` by hand, in the
file's existing alphabetical position. **Each is TWO lines, not one** — that
file is a crate root, so its default module search looks beside itself in
`tests/`, and every one of its 98 modules carries an explicit `#[path]`:

```rust
#[path = "suite/winze_scale_probe.rs"]
mod winze_scale_probe;
```

A bare `mod winze_scale_probe;` does not compile. Copy the shape from
`suite.rs:194-195` (`underworld_lithology_probe`), the nearest neighbour.

Do **not** cherry-pick the commits — they carry 403 commits of context you do
not want.

- [ ] **Step 3: Make them compile against current main**

```bash
cargo check -p hornvale-worldgen --all-targets 2>&1 | tee /tmp/hv-probe-check.txt
```

**A `cargo check` that fails early has enumerated nothing.** Its error list
is a floor, not a total: a failure in an early crate means every downstream
crate is never checked. **Iterate to zero before quoting any count.** This
exact misreading understated The Gossan's blast radius by 2x on sites and 3x
on files.

**Do not go in expecting a particular breakage.** An earlier draft of this
step predicted renames from The Gossan and The Stope/The Drift; the
controller then grepped the five files and found **zero** occurrences of
`MetabolicClass`, `Sunless`, `.slot`, `Endotherm` or `Ametabolic` across all
of them — the probes were authored post-Drift and never used the old
vocabulary. That draft would have sent you hunting for renames that are not
there.

What the controller DID verify (2026-08-26, at `7576eca00`): every symbol the
two largest probes import from `hornvale_worldgen::chamber` —
`BRANCHES_PER_SYSTEM`, `ChamberAddr`, `chamber_exists`, `rung_rank` — still
exists on `main`. So the compile surface may well be empty.

Run the check, read what it actually says, and fix whatever it names. Fix the
call sites; do **not** change what a probe measures.

- [ ] **Step 4: Commit the compile fix separately from the re-run**

```bash
make gate-commit
cargo fmt
git add windows/worldgen/tests/suite.rs windows/worldgen/tests/suite/
git commit -m "harvest(the-sources): The Winze's five probes, compiling against main"
```

- [ ] **Step 5: Run them, and record what they actually say**

Each probe is a single `#[ignore]`d heavy test. Run each explicitly by name:

```bash
cargo nextest run -p hornvale-worldgen --run-ignored all \
  -E 'test(/winze_scale_probe|winze_energy_probe|ore_separation_probe|ore_viability_probe|off_lithology_decorrelation_probe/)' \
  --no-fail-fast 2>&1 | tee /tmp/hv-probe-run.txt
```

These are live-worldgen batteries and are slow. **Run once, inspect many** —
do not re-run to grep a second line.

- [ ] **Step 6: Branch on what the numbers did**

This is a decision rule, not a prediction. The Glasshouse's temperature epoch
landed under these figures and nobody has measured whether they moved.

| what you observe | what to do |
|---|---|
| every pinned figure reproduces | record "re-run reproduces, n=… , date" in each probe's doc comment; metaplan §3.1/§3.2 unchanged |
| a figure moved but the probe's **verdict** stands (e.g. 3.264x → 3.1x; still >1) | update the number in the probe doc, the registry row AND metaplan §3.1/§3.2, all in this commit; the verdict sentence stays |
| a figure moved enough to **flip a verdict** (e.g. the size clause stops being falsified, or an axis pair stops being decoupled) | **STOP and report.** This is a finding that changes the campaign's premises and is not yours to absorb quietly |
| a probe fails to produce a number at all | STOP and report; do not substitute the committed figure |

Never edit a probe's assertion to rescue a figure.

- [ ] **Step 7: Land the three registry rows**

```bash
git diff $(git merge-base main campaign/the-winze) campaign/the-winze \
  -- book/src/frontier/idea-registry.md
```

Three rows: `BIO-subterranean-energy-sources` (new),
`MAP-delving-hazard` (new), `BIO-underworld-has-no-energy` (corrected).
Apply them by hand into main's table, carrying whatever Step 6 measured
rather than the branch's figures where those differ.

```bash
cargo nextest run -p hornvale --test suite -E 'test(registry_ids_are_unique)'
```

`registry_ids_are_unique` is the guard that catches a duplicated row, and it
has caught one twice in two days. Run it explicitly.

- [ ] **Step 8: Commit**

```bash
make gate-commit
cargo fmt
git add -A
git commit -m "measure(the-sources): the Winze probes re-run against main"
git push
```

**Push at this task boundary and every later one.** An unpushed branch is
invisible to the mouth, the chamber, and every peer.

---

### Task 2: The rung evaluation depth

**Files:**
- Modify: `domains/terrain/src/delve.rs`
- Test: in-module `#[cfg(test)]` in the same file

**Interfaces:**
- Consumes: `Band`, `delta_t_range_of(rung) -> (f64, Option<f64>)`,
  `GeothermalGradient`, `HABITABLE_CEILING_K`.

**Three things the controller verified in the tree that every test sketch in
this plan depends on** (`domains/terrain/src/`, at `cccb5c6a7`):

1. **`GeothermalGradient::new(f64) -> GeothermalGradient` is INFALLIBLE** —
   it returns `Self`, not `Result`, and validates with
   `debug_assert!(k_per_km.is_finite() && k_per_km > 0.0)` (`strata.rs:26`).
   Do not write `.expect(...)` after it; that does not compile. Do not pass
   `0.0`; that trips the assert in a debug build, which is what tests are.
2. **`rungs()` INCLUDES `Band::Surface`** — it returns `ALL_RUNGS`, Surface
   first, and its own doc says "Callers that want only the habitation rungs
   filter `Surface` out". Any loop over `rungs()` expecting a `Some` back
   from `rung_evaluation_depth_m` must filter it.
3. **`delta_t_range_of(Band::Surface)` returns `(0.0, Some(0.0))`**, not
   `(_, None)` — a degenerate closed interval at the datum
   (`delve.rs:258-266`). Surface is therefore NOT caught by an
   `if let Some(hi)` filter; only `Nadir` is.

The test module to extend is at `domains/terrain/src/delve.rs:305`.
- Produces:
  `pub fn rung_evaluation_depth_m(rung: Band, gradient: GeothermalGradient, depth_reach_m: f64) -> Option<f64>`
  — the depth at which a rung's conditions are read. `None` for
  `Band::Surface`, which names no chamber.

**The rule, and where it comes from.** `MAP-per-rung-substrate` prescribes
it: *"sample a rung's ΔT **midpoint**, not its top — the top makes rank 0
degenerate."* A band is a ΔT interval above the surface datum, so the depth
is `ΔT / gradient` and varies by vertex.

`Band::Nadir` has an open top (`delta_t_range_of` returns `(low, None)` for
the bottom rung), so it has no midpoint. It is evaluated at
`depth_reach_m` — which is **exactly where every rung is evaluated today**.

- [ ] **Step 1: Write the failing tests**

Add to `domains/terrain/src/delve.rs`'s test module:

```rust
#[test]
fn the_surface_rung_has_no_evaluation_depth() {
    let g = GeothermalGradient::new(25.0);
    assert_eq!(rung_evaluation_depth_m(Band::Surface, g, 800.0), None);
}

#[test]
fn nadir_is_evaluated_at_the_caves_own_reach() {
    // THE POSITIVE CONTROL FOR THE WHOLE CAMPAIGN. Today every rung reads
    // `depth_reach_m`; after the per-rung change the deepest rung still
    // must, so its substrate and moisture are byte-identical across the
    // change and every movement is attributable to a shallower rung.
    let g = GeothermalGradient::new(25.0);
    for reach in [120.0, 800.0, 2500.0] {
        assert_eq!(
            rung_evaluation_depth_m(Band::Nadir, g, reach),
            Some(reach),
            "Nadir must read the cave's own reach, not a midpoint"
        );
    }
}

#[test]
fn a_bounded_rung_is_evaluated_at_its_delta_t_midpoint() {
    let g = GeothermalGradient::new(25.0);
    // `rungs()` INCLUDES `Band::Surface` (it returns ALL_RUNGS, Surface
    // first) and Surface's range is the degenerate `(0.0, Some(0.0))`, so it
    // survives the `hi` filter below and would then panic on the `expect`.
    // Filter it explicitly.
    for rung in rungs().iter().filter(|r| **r != Band::Surface) {
        let (lo, hi) = delta_t_range_of(*rung);
        let Some(hi) = hi else { continue }; // Nadir, covered above
        let depth = rung_evaluation_depth_m(*rung, g, 100_000.0)
            .expect("a habitation rung has an evaluation depth");
        let delta_t = depth * g.get() / 1000.0;
        assert!(
            delta_t > lo && delta_t < hi,
            "{rung:?}: evaluation ΔT {delta_t} is not strictly inside ({lo}, {hi}) \
             — the TOP of a rung is what MAP-per-rung-substrate says makes \
             rank 0 degenerate"
        );
    }
}

#[test]
fn evaluation_depth_never_exceeds_the_caves_reach() {
    // A rung deeper than the cave goes is not a place. Whatever the ΔT
    // midpoint says, the answer is bounded by the column that exists.
    let g = GeothermalGradient::new(25.0);
    for rung in rungs() {
        if let Some(d) = rung_evaluation_depth_m(*rung, g, 150.0) {
            assert!(d <= 150.0, "{rung:?} evaluated at {d} m in a 150 m column");
        }
    }
}

#[test]
fn evaluation_depth_is_monotone_in_the_rung() {
    let g = GeothermalGradient::new(25.0);
    let depths: Vec<f64> = rungs()
        .iter()
        .filter_map(|r| rung_evaluation_depth_m(*r, g, 100_000.0))
        .collect();
    for w in depths.windows(2) {
        assert!(w[0] < w[1], "rung depths must increase with the ladder: {depths:?}");
    }
}
```

- [ ] **Step 2: Run them and confirm they fail for the right reason**

```bash
cargo test -p hornvale-terrain --lib -- delve:: 2>&1 | tail -30
```

Expected: FAIL, `cannot find function rung_evaluation_depth_m`.

**A red from a compile error proves nothing about an assertion.** These five
are the exception the rule allows — the function does not exist yet — so
after Step 3 turns them green, Step 4's mutation is what actually establishes
they can fail.

- [ ] **Step 3: Implement**

```rust
/// The depth at which a rung's conditions are read, metres below the
/// surface — the **ΔT midpoint** of the rung's band, converted through the
/// vertex's own gradient, and never deeper than the cave actually reaches.
///
/// **The midpoint, not the top, and this is `MAP-per-rung-substrate`'s own
/// prescription** rather than a fresh choice: the top of a rung makes its
/// shallowest rank degenerate, because a rung's top ΔT is the next rung's
/// bottom.
///
/// [`Band::Nadir`] has no midpoint — [`delta_t_range_of`] gives it an open
/// top — so it reads `depth_reach_m`, which is where EVERY rung was read
/// before per-rung resolution existed. That makes `Nadir` the positive
/// control for the change: its answer must not move.
///
/// [`Band::Surface`] names no chamber and returns `None`.
/// type-audit: bare-ok(diagnostic-value: depth_reach_m), bare-ok(diagnostic-value: return)
pub fn rung_evaluation_depth_m(
    rung: Band,
    gradient: GeothermalGradient,
    depth_reach_m: f64,
) -> Option<f64> {
    if rung == Band::Surface {
        return None;
    }
    let (lo, hi) = delta_t_range_of(rung);
    let depth = match hi {
        // The open-ended bottom rung: read the column where it actually ends.
        None => depth_reach_m,
        Some(hi) => {
            let midpoint_k = 0.5 * (lo + hi);
            1000.0 * midpoint_k / gradient.get()
        }
    };
    Some(depth.min(depth_reach_m).max(0.0))
}
```

- [ ] **Step 4: Verify green, then demonstrate the tests can fail**

```bash
cargo test -p hornvale-terrain --lib -- delve:: 2>&1 | tail -20
```

Expected: PASS.

Then, per Ruling 2, **find your own mutation** demonstrating that
`a_bounded_rung_is_evaluated_at_its_delta_t_midpoint` distinguishes a
midpoint from a rung edge. Paste the failure message into your report.

- [ ] **Step 5: Commit**

```bash
make gate-commit
cargo fmt
git add domains/terrain/src/delve.rs
git commit -m "feat(terrain): a rung's conditions are read at its ΔT midpoint"
git push
```

---

### Task 3: Per-rung substrate and moisture, additive

**Files:**
- Modify: `windows/worldgen/src/lib.rs` (near `subterranean_substrate_field`,
  currently at line 2960)
- Test: `windows/worldgen/tests/suite/underworld_conditions_probe.rs`

**Interfaces:**
- Consumes: `rung_evaluation_depth_m` (Task 2); `subterranean_substrate`,
  `chamber_moisture` (shipped).
- Produces:
  `pub fn subterranean_substrate_at_rung(surface: Substrate, rung: Band, terrain: &GeneratedTerrain, vertex: Vertex) -> Option<Substrate>`
  and
  `pub fn subterranean_substrate_field_per_rung(geo: &Geosphere, terrain: &GeneratedTerrain, surface: &VertexMap<Substrate>) -> VertexMap<[Option<Substrate>; 6]>`,
  indexed by `Band as usize`.

**This task adds and changes nothing.** `subterranean_substrate_field` keeps
its exact behaviour and its two production call sites keep calling it. Task 9
is where consumers switch. Keeping those apart is what makes Task 9's blast
radius attributable.

**Verify before you start:** confirm the two production call sites are still
`windows/worldgen/src/lib.rs:1500` and `:1797`, and that
`subterranean_substrate_field` still evaluates at
`terrain.cave_at(vertex).map_or(0.0, |cave| cave.depth_reach_m)`. If either
has moved, the tree wins — report it.

- [ ] **Step 1: Write the failing tests**

```rust
#[test]
fn the_deepest_rung_reproduces_todays_per_vertex_reading() {
    // THE POSITIVE CONTROL. `subterranean_substrate_field` reads every
    // vertex at `depth_reach_m`; `rung_evaluation_depth_m` gives Nadir that
    // same depth. So the per-rung field's Nadir entry must equal the old
    // field EXACTLY -- bit-for-bit, not approximately -- on every
    // cave-bearing vertex. A change that moves this has changed something
    // it was not asked to change.
    let (geo, terrain, surface) = fixture_world();
    let old = subterranean_substrate_field(&geo, &terrain, &surface);
    let new = subterranean_substrate_field_per_rung(&geo, &terrain, &surface);
    let mut compared = 0;
    for vertex in geo.vertices() {
        if terrain.cave_at(vertex).is_none() {
            continue;
        }
        let nadir = new.get(vertex)[Band::Nadir as usize]
            .expect("a cave-bearing vertex has a Nadir reading");
        let was = old.get(vertex);
        assert_eq!(
            nadir.temperature_c.to_bits(), was.temperature_c.to_bits(),
            "vertex {vertex:?}: Nadir temperature moved"
        );
        assert_eq!(
            nadir.moisture.to_bits(), was.moisture.to_bits(),
            "vertex {vertex:?}: Nadir moisture moved"
        );
        compared += 1;
    }
    assert!(compared > 100, "only {compared} cave-bearing vertices compared — vacuous");
}

#[test]
fn shallower_rungs_are_cooler_than_the_deepest() {
    // The floor the control above needs. A per-rung field where every rung
    // equalled Nadir would pass the control and mean nothing changed at all.
    let (geo, terrain, surface) = fixture_world();
    let field = subterranean_substrate_field_per_rung(&geo, &terrain, &surface);
    let mut vertices_with_a_spread = 0;
    for vertex in geo.vertices() {
        if terrain.cave_at(vertex).is_none() {
            continue;
        }
        let rungs = field.get(vertex);
        let nadir = rungs[Band::Nadir as usize].expect("Nadir reading");
        if let Some(u) = rungs[Band::Undercroft as usize] {
            if u.temperature_c < nadir.temperature_c {
                vertices_with_a_spread += 1;
            }
        }
    }
    assert!(
        vertices_with_a_spread > 100,
        "only {vertices_with_a_spread} vertices show a shallow/deep temperature \
         spread — the per-rung field has collapsed to the per-vertex one"
    );
}
```

**`fixture_world()` DOES NOT EXIST — the controller invented it.** Write the
sketches against the real idiom, which is in the same file you are editing
(`underworld_conditions_probe.rs:294-320`, the `cave_vertices` helper):

```rust
let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
let artifacts = build_world_to_with_artifacts(
    hornvale_kernel::Seed(42),
    &SkyPins::default(),
    SkyChoice::Generated,
    &TerrainPins::default(),
    &SettlementPins::default(),
    &wc,
    BuildDepth::Terrain,          // <- Terrain, NOT Settlements
).expect("probe seed builds");
let terrain = artifacts.terrain.expect("terrain is Some at BuildDepth::Terrain");
let climate = climate_of(&artifacts.world).expect("climate reconstructs");
let geo = terrain.geosphere();
let surface = substrate_field(
    geo, &terrain, &climate,
    climate.obliquity_deg(), climate.insolation(), &climate.regime(),
);
```

**`BuildDepth::Terrain` is sufficient and is what the neighbouring probe
uses.** Caves, lithology, the geothermal gradient and the substrate field all
exist at that depth; nothing in this task needs settlements. Do not reach for
`BuildDepth::Settlements` — it is strictly more expensive for nothing.

**On `#[ignore]`:** that file's own
`the_live_substrate_field_carries_depth` (line 377) builds a live world at
`BuildDepth::Terrain` and is **NOT** `#[ignore]`d, while two heavier tests in
the same file are. So measure before you decide: if your tests run in
seconds, leave them in the ordinary suite; if they do not, use that file's
exact reason string, which names a cost AND cites a decision:

```rust
#[ignore = "heavy: live-worldgen battery; deferred from the commit gate to the heavy set (decision 0132)"]
```

Two API facts the controller verified so you do not have to:
`VertexMap::get(&self, id: Vertex) -> &T` (`kernel/src/geosphere.rs:61`) —
it returns a REFERENCE, so deref where you compare. And
`GeneratedTerrain::cave_at(&self, id: Vertex) -> Option<Cave>`
(`domains/terrain/src/provider.rs:386`) returns by VALUE, not by reference.
`Band as usize` is already an established idiom here — see
`windows/worldgen/tests/suite/underworld_capacity_probe.rs`.

- [ ] **Step 2: Run and confirm the expected failure**

```bash
cargo nextest run -p hornvale-worldgen --run-ignored all \
  -E 'test(/the_deepest_rung_reproduces|shallower_rungs_are_cooler/)' 2>&1 | tail -25
```

- [ ] **Step 3: Implement**

Mirror `subterranean_substrate_field`'s existing body exactly, replacing the
single `depth_m` with a per-rung one from `rung_evaluation_depth_m`. Derive
`porosity`, `water_table_m` and `gradient` the same way it does — **call the
same helpers rather than re-deriving**, which is the stated reason The
Underworld hoisted them into that function in the first place ("so this call
site and `per_species_capacity_at`'s cannot derive them differently").

A vertex with no cave gets `[None; 6]`.

- [ ] **Step 4: Verify green, then mutate**

Per Ruling 2, find a mutation that makes
`the_deepest_rung_reproduces_todays_per_vertex_reading` fail, and a
*different* one that makes `shallower_rungs_are_cooler_than_the_deepest`
fail. **Both**, because they guard opposite directions — and if one mutation
kills both, they are not independent and one of them is not doing the job its
name claims.

- [ ] **Step 5: Confirm nothing moved**

```bash
make rebaseline
git diff --exit-code -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$')
```

| what you observe | what to do |
|---|---|
| empty diff | expected — this task is additive; record it |
| `docs/audits/` moved | expected if you added a `pub` item: regenerate and commit in the SAME commit |
| anything else moved | **STOP and report.** Ruling 7: only Task 9 moves numbers |

- [ ] **Step 6: Commit**

```bash
make gate-commit
cargo fmt
git add -A
git commit -m "feat(worldgen): read a chamber's conditions per rung"
git push
```

---

### Task 4: The six sources

**Files:**
- Create: `windows/worldgen/src/energy.rs`
- Modify: `windows/worldgen/src/lib.rs` (add `pub mod energy;` — the module
  list is alphabetical, so it goes between `disposition` and `gazetteer`,
  around line 91)
- Test: in-module `#[cfg(test)]`

**Interfaces:**
- Consumes: `MaterialBuffer` (`silica`, `carbonate`, `porosity`,
  `metamorphic_grade`), `GeothermalGradient`, and a rung's moisture (Task 3).
- Produces: `pub enum EnergySource` with a `yield_at` method, and
  `pub const ALL: [EnergySource; 7]` (six from the registry row plus
  `DetritalImport` — see Ruling P1 below). Each yield is `f64` in `[0,1]`.

**The accounting, stated so it is not rediscovered.** The registry row names
six sources; mapped onto shipped axes they are **not six independent
inputs**. Three read `silica` at different bands:

```
serpentinization    ultramafic  -> LOW silica
iron reduction      mafic       -> LOW-MID silica
radiolysis          granite     -> HIGH silica
sulphide oxidation  metamorphic_grade
methanogenesis      carbonate x porosity
geothermal          the gradient (ships)
```

`induration` is **deliberately excluded**: metaplan §3.2 measured
`induration x metamorphic_grade` at **0.9818**, so admitting both
double-counts one signal under two names. `grain` is unused because no source
names it.

**A SEVENTH TERM, which the registry row does NOT name (controller Ruling
P1).** `BIO-subterranean-energy-sources` names six sources and every one is
lithological and chemotrophic. But `domains/climate/src/underworld.rs:65`
says the underworld's supply is **detrital import near the surface AND
chemolithotrophy off the geothermal gradient at depth** — both halves — and
the row names only the half that was missing. Without an import term the sum
has no shallow arm and **cannot** produce Task 5's U, so Task 5 would be
measuring a field structurally incapable of the shape it tests for.

So this module ships **seven** terms: the row's six, plus
`EnergySource::DetritalImport`. Its input is overhead drainage
(`GeneratedTerrain::drainage_at`, `domains/terrain/src/provider.rs:255`) —
what `underworld.rs`'s own axis note calls "overhead `drainage`, what
arrives" — and it **falls** with depth where the six do not. Its doc comment
must state plainly that it is not one of the registry row's six and why it is
here.

Consequence for the tests below: `EnergySource::ALL` has **seven** entries,
and `the_three_silica_sources_peak_at_different_silica` is unaffected because
`DetritalImport` reads no silica.

**Each source's doc comment names the rock and the reaction**, in the
derivation-comment discipline `domains/climate/src/underworld.rs`'s corpus
already uses. That is what makes the table auditable rather than tasteful.

- [ ] **Step 1: Write the failing tests**

```rust
#[test]
fn every_source_is_a_ratio() {
    // Each source lands in [0,1] over the whole input domain, including the
    // degenerate corners. A source that can exceed 1 would let the sum leave
    // the ENERGY ruler, which EnvironmentVector::new rejects outright.
    for silica in [0.0, 0.25, 0.5, 0.75, 1.0] {
        for other in [0.0, 0.5, 1.0] {
            let m = buffer(silica, other, other, other);
            // NOT 0.0: `GeothermalGradient::new` carries
            // `debug_assert!(k_per_km.is_finite() && k_per_km > 0.0)`, and
            // tests run in debug. Use a small positive value at the cold end.
            for g in [1.0, 25.0, 120.0] {
                let grad = GeothermalGradient::new(g);
                for source in EnergySource::ALL {
                    let v = source.yield_at(&m, grad, 500.0, other);
                    assert!(
                        (0.0..=1.0).contains(&v) && v.is_finite(),
                        "{source:?} returned {v} for silica={silica} other={other} grad={g}"
                    );
                }
            }
        }
    }
}

#[test]
fn the_three_silica_sources_peak_at_different_silica() {
    // The reduction above is honest only if the three bands are actually
    // distinct. If two peak together they are one source with two names.
    let peak = |s: EnergySource| {
        let grad = GeothermalGradient::new(25.0);
        (0..=100)
            .map(|i| i as f64 / 100.0)
            .max_by(|a, b| {
                s.yield_at(&buffer(*a, 0.5, 0.5, 0.5), grad, 500.0, 0.5)
                    .total_cmp(&s.yield_at(&buffer(*b, 0.5, 0.5, 0.5), grad, 500.0, 0.5))
            })
            .expect("a non-empty sweep")
    };
    let serp = peak(EnergySource::Serpentinization);
    let iron = peak(EnergySource::IronReduction);
    let radio = peak(EnergySource::Radiolysis);
    assert!(
        serp < iron && iron < radio,
        "the three silica bands must be ordered ultramafic < mafic < granitic, \
         got serpentinization={serp}, iron={iron}, radiolysis={radio}"
    );
    assert!(
        (iron - serp).abs() > 0.1 && (radio - iron).abs() > 0.1,
        "two silica sources peak within 0.1 of each other — they are one \
         source under two names, and the six-source claim is not honest"
    );
}

#[test]
fn a_water_rock_reaction_needs_water() {
    // Serpentinization and methanogenesis are water-rock reactions. In a dry
    // chamber they yield nothing, whatever the rock says.
    let grad = GeothermalGradient::new(25.0);
    let rich = buffer(0.1, 0.9, 0.9, 0.9);
    for source in [EnergySource::Serpentinization, EnergySource::Methanogenesis] {
        let wet = source.yield_at(&rich, grad, 500.0, 1.0);
        let dry = source.yield_at(&rich, grad, 500.0, 0.0);
        assert!(wet > 0.0, "{source:?} yields nothing even wet — check the rock inputs");
        assert_eq!(dry, 0.0, "{source:?} yields {dry} in a chamber with no water");
    }
}

#[test]
fn sulphide_oxidation_peaks_at_intermediate_depth() {
    // It needs oxidant from above meeting sulphide from below, so it is the
    // one source that is neither rising nor falling in depth. This is what
    // makes the U's trough possible rather than imposed.
    let grad = GeothermalGradient::new(25.0);
    let m = buffer(0.5, 0.5, 0.5, 0.8);
    let at = |d: f64| EnergySource::SulphideOxidation.yield_at(&m, grad, d, 0.5);
    let depths: Vec<f64> = (0..=20).map(|i| i as f64 * 100.0).collect();
    let best = depths
        .iter()
        .copied()
        .max_by(|a, b| at(*a).total_cmp(&at(*b)))
        .expect("a non-empty sweep");
    assert!(
        best > 0.0 && best < 2000.0,
        "sulphide oxidation peaked at {best} m — an endpoint peak means it is \
         monotone, not an intermediate-depth redox front"
    );
}
```

`buffer(silica, carbonate, porosity, metamorphic_grade)` is a local test
helper building a `MaterialBuffer`; write it in the test module.

**`MaterialBuffer` has ten fields, not four** — `silica`, `grain`,
`induration`, `carbonate`, `metamorphic_grade`, `porosity`, `thaumic`, plus
`margin: MarginPolarity`, `soil_depth: SoilDepth` and `basement: Basement`
(`domains/terrain/src/lithology.rs:84`). Your four-argument helper fills the
four this module reads and picks fixed values for the rest; say in the report
which fixed values you chose, because a source that turns out to read one of
them would silently be reading a constant.

It derives `Debug, Clone, Copy, PartialEq`, so pass it by value or reference
as convenient. Two existing test constructors show the shape —
`domains/terrain/src/features.rs:715` (`fn buf(carbonate, silica)`) and
`domains/terrain/src/cave_depth.rs:229` (`fn karstic()`) — but **both live in
private test modules inside another crate and are not reachable from
`windows/worldgen`**. Read them as models; write your own.

- [ ] **Step 2: Run and confirm failure**

```bash
cargo test -p hornvale-worldgen --lib -- energy:: 2>&1 | tail -30
```

- [ ] **Step 3: Implement the six sources**

Each is a pure function with **no transcendental** unless it routes through
`hornvale_kernel::math` (decision 0041). Prefer the shape `chamber_moisture`
chose — continuous, monotone where it should be, and rational rather than
exponential — for the same stated reason: an exponential "would read the same
and would have to route through `hornvale_kernel::math` for determinism,
buying nothing".

A silica band is a triangular or smoothstep window centred on its rock class.

**`smoothstep` exists twice in this tree and you can reach NEITHER.**
`kernel/src/noise.rs:18` and `domains/terrain/src/rift.rs:315` are both
private `fn`s, and `domains/alchemy`'s `clamp01` is `pub(crate)`. Write a
private helper in `energy.rs` rather than hunting for a reusable one or
promoting somebody else's — a smoothstep is a three-term polynomial, and
promoting a kernel private to serve one caller in `windows/` is a layering
change this task has no mandate for. **Say in your report that you wrote a
third copy deliberately and why**, so the reviewer does not flag it as
duplication it should have reused; the controller has already checked that
there is nothing to reuse.

If you do want a transcendental after all, `hornvale_kernel::math` exposes
`exp`, `powf`, `ln` and `tanh` (decision 0041 routes them through the pure-Rust
`libm` for cross-platform bit-identity). Prefer the polynomial anyway, for
the reason `chamber_moisture`'s doc gives about its own shape.

The three centres must be far enough apart that
`the_three_silica_sources_peak_at_different_silica` passes on its own terms —
if it does not, that is a finding about spec §4.1's reduction and you should
report it, **not widen the tolerance**.

- [ ] **Step 4: Verify green, then mutate**

Per Ruling 2, find a mutation demonstrating that
`the_three_silica_sources_peak_at_different_silica` would catch two sources
collapsing onto one band.

- [ ] **Step 5: Commit**

```bash
make gate-commit
cargo fmt
git add windows/worldgen/src/energy.rs windows/worldgen/src/lib.rs
git commit -m "feat(worldgen): six named subterranean energy sources"
git push
```

---

### Task 5: The energy field, and the U it must reproduce

**Files:**
- Modify: `windows/worldgen/src/energy.rs`
- Create: `windows/worldgen/tests/suite/subterranean_energy_probe.rs`
- Modify: `windows/worldgen/tests/suite.rs`

**Interfaces:**
- Consumes: Task 4's `EnergySource`; Task 2's `rung_evaluation_depth_m`;
  Task 3's per-rung moisture.
- Produces:
  `pub fn subterranean_energy(material: &MaterialBuffer, gradient: GeothermalGradient, depth_m: f64, moisture: f64, drainage: f64) -> f64`
  (the `ENERGY` ruler, `[0,1]`),
  `pub fn dominant_source(material: &MaterialBuffer, gradient: GeothermalGradient, depth_m: f64, moisture: f64, drainage: f64) -> EnergySource`,
  and
  `pub fn subterranean_energy_field_per_rung(geo: &Geosphere, terrain: &GeneratedTerrain, surface: &VertexMap<Substrate>) -> VertexMap<[Option<f64>; 6]>`.

**Two API facts the controller verified against the shipped module** (at
`f53895693`), because the sketches below depend on both:

1. **`EnergySource` does NOT derive `Ord`** — only
   `Debug, Clone, Copy, PartialEq, Eq` (`energy.rs:260`). So it cannot key a
   `BTreeMap`, and `HashMap` is banned workspace-wide. **Do not add an `Ord`
   derive to make a map work.** These seven are *nominal* categories, not a
   ranking — the kernel draws exactly this distinction with
   `AxisValence::Nominal` ("the numeric value indexes an unordered set and is
   never a magnitude"), and an `Ord` on them would assert a precedence that
   does not exist. Tally into a fixed `[usize; EnergySource::ALL.len()]`
   indexed by position in `ALL`, which is what the sketch does.
2. **`yield_at` takes FIVE arguments** as shipped —
   `(&self, buffer: &MaterialBuffer, gradient: GeothermalGradient, depth_m: f64, moisture: f64, drainage: f64)`
   (`energy.rs:341`). `drainage` was added by a controller ruling in Task 4's
   fix round; `dominant_source` and `subterranean_energy` must take it too,
   and the field function must supply it from
   `GeneratedTerrain::drainage_at`.

Task 3's shipped names, which this task consumes:
`subterranean_substrate_at_rung` (`lib.rs:3013`) and
`subterranean_substrate_field_per_rung` (`lib.rs:3049`).

**The claim this task is measured against, and it is not this campaign's
claim.** `domains/climate/src/underworld.rs`'s module doc states that
underground the supply is detrital import near the surface and
chemolithotrophy off the geothermal gradient at depth, so **energy inverts
with depth**; `energy_is_not_monotone_in_depth` asserts the *shape* — the
mid-ladder `Deeps` trough strictly below both halves.

**The U must EMERGE from the sum, not be built into it.** No term is shaped
to make a trough. The shallow arm is detrital import (the one
non-chemotrophic term); the deep arm is the geothermal source; sulphide
oxidation supplies the intermediate-depth bump.

`dominant_source` is what preserves the registry row's claim that the
*differences* between sources matter: one scalar on the ruler, with the kind
of place retrievable beside it. This is `MARINE_FORAGE`'s precedent applied
deliberately — one axis, one calibration knob, distinction retained rather
than discarded.

- [ ] **Step 1: Write the test that measures the shape**

```rust
/// **Does the DERIVED field reproduce the AUTHORED corpus's central claim?**
///
/// `domains/climate/src/underworld.rs` authors energy for 22 communities and
/// its `energy_is_not_monotone_in_depth` asserts the trough sits at `Deeps`.
/// This asks the same question of a field derived from rock, with nothing
/// shaped to produce the answer.
///
/// **A falsification here is a FINDING, not a failure** (decision 0016).
/// Record the measured profile in this doc comment with today's date and
/// change the assertion; never retune a source to rescue the prediction.
#[test]
#[ignore = "heavy: live-worldgen battery over the frozen seed set"]
fn derived_energy_troughs_in_the_ladders_middle() {
    let mut profile: [Vec<f64>; 6] = Default::default();
    for seed in SEEDS {
        let (geo, terrain, surface) = world_at(seed);
        let field = subterranean_energy_field_per_rung(&geo, &terrain, &surface);
        for vertex in geo.vertices() {
            for (i, e) in field.get(vertex).iter().enumerate() {
                if let Some(e) = e {
                    profile[i].push(*e);
                }
            }
        }
    }
    let median = |v: &mut Vec<f64>| {
        v.sort_by(f64::total_cmp);
        v[v.len() / 2]
    };
    let m: Vec<f64> = profile.iter_mut().map(median).collect();
    let deeps = m[Band::Deeps as usize];
    let shallow = m[Band::Undercroft as usize].min(m[Band::Shallows as usize]);
    let deep = m[Band::Underdeep as usize].min(m[Band::Nadir as usize]);
    assert!(
        deeps < shallow && deeps < deep,
        "PREREGISTERED PREDICTION FALSIFIED (spec §4.3): derived energy does \
         not trough at Deeps. Profile by rung: {m:?}. The authored corpus \
         asserts this inversion (energy_is_not_monotone_in_depth); a derived \
         field that disagrees is a FINDING about one of the two. Record the \
         profile above with today's date and change this assertion — do NOT \
         retune a source."
    );
}
```

- [ ] **Step 2: Run it and record the profile whichever way it goes**

```bash
cargo nextest run -p hornvale-worldgen --run-ignored all \
  -E 'test(derived_energy_troughs_in_the_ladders_middle)' 2>&1 | tail -30
```

| what you observe | what to do |
|---|---|
| troughs at `Deeps` | record the six medians in the doc comment with the date; this is the headline |
| non-monotone but troughs elsewhere | record it, change the assertion to the measured rung, and **say so in your report** — the derived and authored fields disagree about *where*, which is a real finding |
| monotone in depth | record it and change the assertion to what was measured. **STOP and report** — the derived field contradicts a shipped, tested claim |

In all three rows: **do not touch Task 4's sources.** Post-unblinding retunes
are what preregistration exists to prevent, and three defensible ones mean a
question is not resolved.

- [ ] **Step 3: Add the dominant-source test**

```rust
/// If a single source dominates every chamber in every world, the other five
/// are decoration and `BIO-subterranean-energy-sources`'s claim that the
/// DIFFERENCES motivate ecology, trade, exploration and mining is not yet
/// true of the code, whatever the source functions say individually.
#[test]
#[ignore = "heavy: live-worldgen battery over the frozen seed set"]
fn more_than_one_source_dominates_somewhere() {
    // A FIXED-SIZE TALLY INDEXED BY `EnergySource::ALL` POSITION, not a
    // BTreeMap. `EnergySource` derives `Debug, Clone, Copy, PartialEq, Eq`
    // and deliberately NOT `Ord` — see the controller note below.
    let mut histogram = [0usize; EnergySource::ALL.len()];
    let mut chambers = 0usize;
    for seed in SEEDS {
        let (geo, terrain, surface) = world_at(seed);
        let moisture = subterranean_substrate_field_per_rung(&geo, &terrain, &surface);
        for vertex in geo.vertices() {
            let Some(cave) = terrain.cave_at(vertex) else { continue };
            let m = terrain.material_at(vertex);
            let g = terrain.geothermal_gradient_at(vertex);
            for rung in rungs() {
                let Some(depth) = rung_evaluation_depth_m(*rung, g, cave.depth_reach_m)
                else { continue };
                let Some(sub) = moisture.get(vertex)[*rung as usize] else { continue };
                let winner = dominant_source(&m, g, depth, sub.moisture, drainage);
                let slot = EnergySource::ALL
                    .iter()
                    .position(|s| *s == winner)
                    .expect("a dominant source is one of ALL");
                histogram[slot] += 1;
                chambers += 1;
            }
        }
    }
    assert!(chambers > 10_000, "only {chambers} chambers sampled — vacuous");
    // Report BEFORE asserting: the histogram is the finding, the assertion
    // is only its floor.
    println!("dominant-source histogram over {chambers} chambers: {histogram:?}");
    let occupied = histogram.iter().filter(|n| **n > 0).count();
    assert!(
        occupied > 1,
        "one source dominates every chamber in every world ({histogram:?} \
         over {:?}). The other six are decoration. Record this in the \
         chronicle as measured — do NOT retune a source to spread the \
         histogram.",
        EnergySource::ALL
    );
}
```

Its result is a finding, not a gate. If one source dominates everywhere,
record the histogram and say so plainly in the chronicle; the floor asserted
here is deliberately the weakest one that is not vacuous.

- [ ] **Step 3b: Carry Task 4's one deferred minor**

While you are in `energy.rs`, amend `GEOTHERMAL_REACH_K`'s doc. Task 4's
re-review judged its anchor **real but interpretive** — it closed a circular
justification, but unlike `DETRITAL_IMPORT_DRAINAGE_REACH` (which cites a
measured p90 drainage statistic) nothing independently measures where
geothermal supply *should* cross half-yield. It borrows `Underdeep`'s
boundary, which was built to classify habitability, not to calibrate
geothermal yield. The verdict was that the doc's confidence "slightly outruns
its evidentiary weight".

Add one sentence saying plainly that this is a **modelling choice, not an
independent citation**, and that Task 5's measured profile is what would
revise it. Do not remove the anchor and do not weaken the others — the two
kinds of constant now sit side by side in this module and a reader should be
able to tell which is which.

- [ ] **Step 4: Verify, mutate, commit**

```bash
make gate-commit
cargo fmt
git add -A
git commit -m "feat(worldgen): energy over the rock, per rung"
git push
```

---

### Task 6: Does energy vary BETWEEN worlds?

**Files:**
- Modify: `windows/worldgen/tests/suite/subterranean_energy_probe.rs`

**Interfaces:**
- Consumes: Task 5's `subterranean_energy_field_per_rung`.
- Produces: TWO numbers rung 3 inherits — the between-worlds separation
  (frozen in the spec) and the within-world width (frozen below, 2026-08-26).

**Frozen in the spec before any of this existed** (spec §6), and reproduced
here verbatim so the implementer does not have to interpret it:

```
E_s        = per-rung subterranean energy over all cave-bearing vertices of seed s
m_s        = median(E_s)
S          = {1, 7, 42, 99, 123, 256, 512, 777, 1024, 1234, 4096, 9001}   (n = 12)

separation = IQR({ m_s : s in S }) / median({ IQR(E_s) : s in S })

PREDICTION: separation >= 0.25
```

**Why it is in doubt.** The Winze's Task 1 assumed ore prospectivity varied
usefully across the map and measured 75% of all land inside a band 0.0067
wide. `mineral_supply_field` reads that very quantity.

### The SECOND question, frozen 2026-08-26 (Nathan's ruling)

**This is a NEW question, not a revision of anything.** Task 5 measured
whether the field troughs in the ladder's middle; it does not, and that
result stands recorded and untouched. Nathan's ruling on reading it:
**nothing should be dead by default, but there must be a lot of variation** —
"dry, dusty hallways where nothing has moved for generations and lush,
richly carpeted fungal forests". A monotone median profile is compatible
with both a world of identical chambers and a world of wildly different ones,
and **everything measured so far is a median**, so nothing yet distinguishes
them.

The precedent for taking this seriously is the same one §6 already cites: ore
prospectivity was assumed to vary usefully and measured 75% of all land
inside a band **0.0067** wide.

**Criterion S1, frozen before the code that would move it.** For rung `r`
and seed `s`, let `E(r,s)` be the energy values over all cave-bearing
vertices. Over the same frozen seed set `S`:

```
    width(r) = median({ p90(E(r,s)) - p10(E(r,s)) : s in S })

    PREDICTION: width(r) >= 0.25 at every underground rung
```

**Why 0.25 and not a number I liked.** The `ENERGY` ruler is shared with
`domains/climate/src/underworld.rs`'s authored corpus, whose five levels sit
exactly 0.25 apart — `E_INERT` 0.0, `E_LEAN` 0.25, `E_FED` 0.5, `E_RICH`
0.75, `E_TEEMING` 1.0. A p10–p90 width below one full band means the middle
80% of chambers at that depth all read as the same authored level: not
different kinds of place, one kind of place with rounding. The threshold is
read off the existing table, not chosen.

**Its blind zone, named rather than discovered.** p10–p90 discards both
tails, so a world where 95% of chambers are identical and 5% are
extraordinary reads as narrow — and that world is arguably exactly what
"dusty hallways and fungal forests" describes. **So also report p1, p50, p99
and min/max per rung**, and report them *before* the verdict.

**Criterion S2 — is the field even on its own ruler? (diagnostic, not a
prediction.)** Report the share of chambers falling in each of the corpus's
five bands, per rung:

```
    [0, 0.125)  inert     [0.125, 0.375)  lean     [0.375, 0.625)  fed
    [0.625, 0.875)  rich   [0.875, 1.0]  teeming
```

Task 5 measured per-rung medians of 0.169–0.281 under the shipped `mean`
aggregation, so **the upper bands may be structurally unreachable**. If they
are, that is a *calibration* finding distinct from a variation finding: a
field that only ever produces the bottom third of a `[0,1]` ruler cannot be
compared to a corpus that authors values across the whole of it. Report it
plainly either way; do not rescale anything to fill the bands.

**Criterion S3 — variety of KIND, not just amount (diagnostic).** Report the
`dominant_source` histogram per rung. Task 5's diagnostic already found all
seven sources occupied overall, with `SulphideOxidation` taking the deep end
(0 → 1705 of 3821) and `DetritalImport` winning only 6.2% even at its best
rung. Two chambers with the same energy but different dominant sources are
different kinds of place, and that is variation the width statistic cannot
see.

**All three are reported before any verdict is drawn, and none of them may
retune a source.** A narrow field is a finding rung 3 must inherit, exactly
as a null on the between-worlds statistic would be.

- [ ] **Step 1: Write both measurements exactly as the formulae above**

Report the twelve per-seed medians **individually, before the verdict**. The
spec names this criterion's blind zone: IQR-of-medians cannot see eleven
worlds agreeing and one being extraordinary.

- [ ] **Step 2: Run it — and check the cost before assuming it is heavy**

**Build to `BuildDepth::Terrain`, not `Settlements`** (see Task 3's note).
Everything this measures — caves, lithology, the gradient, the substrate
field — exists at `Terrain` depth, and the neighbouring
`underworld_conditions_probe` builds live worlds at that depth in a test it
does not even mark `#[ignore]`. An earlier draft of this step called twelve
worlds "a heavy battery" and priced it at `Settlements`; that was an
assumption, not a measurement.

Time one seed first, then multiply. If twelve seeds genuinely exceed a few
minutes it is a `heavy:` tier test and belongs behind
`make heavy-remote REF=<full-sha>` — the heavy tier is an *authoring* path
with a canonical-host guard, dispatched with a full SHA, never a branch
name.

- [ ] **Step 3: Record the result either way**

| what you observe | what to do |
|---|---|
| `separation >= 0.25` | record the number and the twelve medians; rung 3's premise stands |
| `separation < 0.25` | record the number and the twelve medians. **This is the campaign's most valuable output**, not a failure: rung 3 cannot rest "not every world is *DOOM*" on this quantity and must inherit the finding. Report it prominently and put it in the chronicle's headline |

Either way the committed assertion pins **what was measured**, with the date,
so a later campaign can tell drift from disagreement.

- [ ] **Step 4: Commit and push**

---

### Task 7: `CHEMOSYNTHATE`

**Files:**
- Modify: `kernel/src/ecology.rs`
- Modify: `windows/worldgen/src/lib.rs:1160` (`SUPPLY_AXIS_ORDER`) and both
  capacity loops' `per_axis` arrays
- Test: `kernel/` in-module tests

**Interfaces:**
- Produces: `pub const CHEMOSYNTHATE: ResourceAxis` — **id 6**,
  `ResourceKind::Field`.

**A resource-axis id is a save-format contract.** The basis is append-only
for positional reasons the kernel's own doc explains. Verified at
`7576eca00`: ids 0-5 are taken (`PHOTOSYNTHATE`, `PLANT_FORAGE`,
`ANIMAL_PREY`, `DETRITUS`, `MINERAL`, `MARINE_FORAGE`); **6 is the next free
id and nothing is renumbered.**

`Field` rather than `Stock`, matching `PHOTOSYNTHATE`: ambient primary
production, not standing biomass.

**Verify before you start:** re-read the basis and confirm 6 is still free.
If another campaign has landed an axis, take the next free id and report it —
**do not renumber theirs.**

- [ ] **Step 1: Write the failing tests**

```rust
#[test]
fn chemosynthate_takes_the_next_free_id_and_is_ambient() {
    assert_eq!(CHEMOSYNTHATE.id, 6);
    assert_eq!(CHEMOSYNTHATE.kind, ResourceKind::Field);
}
```

**The append-only guard already exists — EXTEND it, do not add a second.**
`the_basis_ids_are_append_only` (`kernel/src/ecology.rs:668`) reads
`v1_basis()` — note the name: the resource accessor is `v1_basis`, **not**
`resource_v1_basis`; only the environment one carries a qualifier
(`environment_v1_basis`) — and asserts a hard-coded list:

```rust
    let ids: Vec<u16> = v1_basis().iter().map(|a| a.id).collect();
    assert_eq!(ids, vec![0, 1, 2, 3, 4, 5], ...);
```

**That test is a designed tripwire and it WILL go red on this task.** That is
correct behaviour, not an obstacle: it exists so that no axis reaches the
basis without someone consciously acknowledging the save-format consequence.
Change the literal to `vec![0, 1, 2, 3, 4, 5, 6]` and **read its comment
first** — it explains that the obvious pin (a zero-weight axis contributing an
exact zero) catches nothing, because `x + 0.0 == x` at any position, so a
prepend or a mid-slice insert leaves every sum bit-identical. Adding a second
test asserting density would be that same useless pin under a new name.

Add `CHEMOSYNTHATE` to the `v1_basis()` slice itself, **at the end**.

- [ ] **Step 2: Run, implement, verify**

- [ ] **Step 3: Extend `SUPPLY_AXIS_ORDER` to seven**

`SUPPLY_AXIS_ORDER` is `[ResourceAxis; 6]` at
`windows/worldgen/src/lib.rs:1161` and its **order is load-bearing**: it must
match the `per_axis` arrays in *both* capacity loops. Append `CHEMOSYNTHATE`
at the end, widen the type to `[_; 7]`, and add the matching entry to **both**
`per_axis` arrays — currently at `lib.rs:1559` and `lib.rs:1846`.

**THE GUARD THAT PINS THIS PARSES SOURCE TEXT, AND IT WILL BITE YOU.**
`the_supply_axis_order_matches_both_capacity_loops` (`lib.rs:10273`)
`include_str!`s its own file, splits on the literal `"let per_axis = ["`,
keeps only blocks containing `(vertex)`, and translates identifiers to axis
labels through a **hardcoded match arm**:

```rust
    "PHOTOSYNTHATE" => "photosynthate",
    "PLANT_FORAGE"  => "plant forage",
    ...
    other => other,
```

**You must add `"CHEMOSYNTHATE" => "chemosynthate"` to that match.** Without
it the identifier falls through `other => other` as the string
`"CHEMOSYNTHATE"`, which will not equal the axis's `label`, and the test
fails with *"a capacity loop's per_axis order drifted"* — a message that
sends you hunting for an ordering bug that does not exist. So give the new
axis the label **`"chemosynthate"`** (lowercase, matching its siblings'
convention) and add the arm.

Note also there are **three** `let per_axis = [` occurrences in that file, not
two — the third is the test module's own fixture at `lib.rs:10247`. The
`(vertex)` filter is what excludes it, and the test asserts `found == 2`. Do
not "fix" that count.

For now the new entry's supply is `0.0` at every vertex — Task 9 wires the
real field. **A zero weight is an exact IEEE-754 no-op** (`x + 0.0 == x`
exactly, at every position), so this step must move no number.

- [ ] **Step 4: Confirm nothing moved**

```bash
cargo nextest run --workspace --no-fail-fast > /tmp/hv-t7.log 2>&1; echo "exit=$?"
grep -E '^ *Summary|FAILED|panicked' /tmp/hv-t7.log
make rebaseline && git diff --exit-code -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$')
```

| what you observe | what to do |
|---|---|
| empty diff except `docs/audits/` | expected; commit the regenerated report in the same commit |
| any world number moved | **STOP.** A zero-weight append cannot move a number; something else changed and you need to find out what before proceeding |

- [ ] **Step 5: Commit and push**

This is a **kernel-layer** edit, so `make gate-commit` is at its most
expensive here (it rebuilds the most compilation units). Budget for it; do
not skip it.

---

### Task 8: Chemotrophy becomes witnessed

**Files:**
- Modify: `domains/species/src/lib.rs` (the `xorn` row, at line 3417) — its
  `trophic_mode` ONLY; its `niche` is Task 9's (Ruling P2)
- Modify: `domains/species/tests/suite/metabolic_pairs.rs`

**Interfaces:**
- Consumes: `CHEMOSYNTHATE` (Task 7).
- Produces: a registry in which exactly one kind carries
  `TrophicMode::Chemotrophic`.

**The witness is `xorn`, and no new kind is authored.** Its row is already
`ThermalStrategy::Absent` + `TrophicMode::Absent` with niche `MINERAL 1.0`
and the authored comment "Ametabolic (both axes `Absent`), burrows through
stone: lives IN the substrate, not on it". A thing that burrows through stone
and eats only mineral is a chemolithotroph; `Absent`/`Absent` was the honest
encoding available before the variant existed.

**Verified at `7576eca00`: the label change alone moves no number.**
- `is_ametabolic(thermal) -> thermal == Absent` reads the THERMAL axis only
  (`domains/species/src/lib.rs:2368`), so `xorn` stays ametabolic, BMR stays
  0.0, and `life-history-all-kinds.txt` does not move.
- `prey_pressure_from` — the trophic axis's only production reader — filters
  `trophic_mode != Phototrophic` (`windows/worldgen/src/lib.rs:2145`), so
  `Absent` and `Chemotrophic` are treated identically.

**Re-verify both before relying on them.** They are this task's entire safety
argument, and a safety argument nobody re-ran is a claim, not a fact.

- [ ] **Step 1: Change the row, and give it something to eat**

`trophic_mode: TrophicMode::Absent` → `TrophicMode::Chemotrophic`.
`thermal_strategy` is **unchanged**.

**The `niche` is NOT touched in this task (controller Ruling P2).** The
obvious move — give xorn a `CHEMOSYNTHATE` weight here — would move a shipped
number in a task Ruling 7 declares behaviour-preserving, and would move it in
the *wrong direction*: Task 7 pinned that supply at `0.0` everywhere and Task
9 has not yet wired the real field, so between here and there xorn would be a
chemotroph eating a field of zeros and its capacity would simply drop.
Nothing in this task would catch that — its only artifact check is the
life-history fixture, which BMR holds still.

So the niche edit lands in **Task 9**, in the same commit as the supply that
feeds it. **Say in your report that the gap is open and owned by Task 9** — a
witnessed variant nothing reads is exactly the rot The Gossan documented
(`Autotroph` "witnessed by The Menagerie without the modelling decision ever
being made", still an unused seam three campaigns later), and it is
acceptable here only because it closes one task later by construction.

- [ ] **Step 2: The two handoff tests The Gossan left**

`chemotrophic_is_declared_and_unwitnessed` — its assertion must now change.
Its own message says "If rung 2 has landed, this assertion is what you came
to delete." **Ruling 4 still applies: keep the name.** Invert the body to
assert `xorn` and only `xorn` carries the variant, and rewrite the doc
comment to state what it now guards and in which direction.

`sanctioned_thermal_keys_are_pairwise_distinct` — adding
`(Absent, Chemotrophic)` duplicates the thermal key of the already-sanctioned
`(Absent, Absent)`, which is precisely what this test predicted rung 2 would
do. Its message is explicit:

> RUNG 2 OF THE UNDERWORLD LARDER IS THE EDIT THAT BREAKS THIS ... do not
> simply delete this test — replace it with a direct per-kind pin on
> `trophic_mode`, because that is the guard this property was standing in
> for.

**Replace, do not delete.** The Gossan's review demonstrated by mutation that
with the thermal key duplicated, `every_kind_carries_a_sanctioned_pair`,
`metabolic_class_coverage_matches_the_table` and the life-history golden all
stay green while the trophic axis moves.

**The controller verified that claim rather than passing it on, and it is
true in a stronger form than The Gossan stated.**
`domains/species/tests/suite/coverage.rs` contains **zero** occurrences of
`trophic_mode` or `TrophicMode` — its table is
`&[(ThermalStrategy, Rung, &[&str])]` and it pins the THERMAL axis only. So
after you duplicate the `Absent` thermal key, the replacement per-kind pin
you write is **the only guard on the trophic axis anywhere in the suite**.
That makes it the most load-bearing thing in this task, not a formality.

Two consequences you can rely on:
- `metabolic_class_coverage_matches_the_table` needs **no change** — it never
  reads the axis you are moving.
- `(ThermalStrategy::Absent, Rung::Witnessed, &["xorn"])`
  (`coverage.rs:143`) must stay true: `xorn` is the sole `Absent`-thermal
  witness and its thermal strategy is unchanged. The replacement is an explicit table
of `(kind, trophic_mode)` covering **all** kinds, asserted exhaustively —
exhaustive so that a kind added later cannot slip past it unnamed.

Keep the test's name (Ruling 4). Rewrite its body and its doc comment; the
doc must **name the direction** the new check enforces, per the discipline
`is_ametabolic`'s doc uses.

- [ ] **Step 3: Update `SANCTIONED` and its count assertion**

The `assert_eq!(SANCTIONED.len(), 4, ...)` says "if this count moved, re-read
this test's doc before adjusting the number". Re-read it, then move it to 5.

- [ ] **Step 4: Run the tests you changed, explicitly**

```bash
cargo nextest run -p hornvale-species > /tmp/hv-t8.log 2>&1; echo "exit=$?"
grep -E '^ *Summary|FAILED|panicked|metabolic' /tmp/hv-t8.log
```

Per Ruling 5, these are tests written in this commit; a green `gate-commit`
says nothing about them.

- [ ] **Step 5: Confirm the golden did NOT move**

```bash
git diff --exit-code -- domains/species/tests/fixtures/
```

| what you observe | what to do |
|---|---|
| fixture unchanged | expected — `is_ametabolic` reads the thermal axis only |
| fixture moved | **STOP and report.** The safety argument above is wrong and this task's premise fails |

- [ ] **Step 6: Mutation control**

Per Ruling 2, find a mutation demonstrating the *replacement* per-kind pin
catches a trophic-axis change that the old pair table would have let through.
That is the whole reason the replacement exists, and if it cannot be
demonstrated, the replacement is not doing the job its predecessor's message
asked for.

- [ ] **Step 7: Commit and push**

```bash
make gate-commit
cargo fmt
git add -A
git commit -m "feat(species): a xorn eats rock — Chemotrophic is witnessed"
git push
```

---

### Task 9: The switch — consumers read per rung, and read chemosynthate

**Files:**
- Modify: `windows/worldgen/src/lib.rs` (both capacity loops — the `per_axis`
  arrays are at **1561** and **1853**, and the `CHEMOSYNTHATE` entries
  currently reading `(CHEMOSYNTHATE, 0.0)` are at **1571** and **1863**)
- Modify: `domains/species/src/lib.rs` — xorn's `niche` (deferred from Task 8
  by Ruling P2)
- Modify: `book/src/frontier/idea-registry.md` (`MAP-per-rung-substrate`)

**Interfaces:**
- Consumes: Tasks 3, 5, 7, 8.
- Produces: the campaign's only movement in shipped world numbers.

**This is the only task that moves a shipped world's numbers** (Ruling 7).
Two things change at once and that is deliberate: doing them separately would
mean measuring the blast radius twice and attributing it neither time.

**The rule for which rung a species reads: the best rung.** A subterranean
kind is credited with `max` over the column's rungs of the suitability
computed **at that rung** — the max of the whole per-rung score, not the rung
with the most energy, and **not** a per-axis max, which would assemble a
chimeric place that exists at no rung. This is what
`delve_seating::seat_at` already assumes when it picks a rung per candidate.
A mean over rungs would let a column of five hostile rungs and one excellent
one score below a uniformly mediocre column, inverting the seating logic that
reads the result.

`availability` (cave presence) and `affinity` (biome) are rung-independent
and stay exactly where they sit today — **read the long comment around
`windows/worldgen/src/lib.rs:1580` on why each sits outside the Liebig
minimum before moving anything.** One is a presence mask in `{0,1}` and one
is a graded factor, and the comment explains why that distinction is
load-bearing rather than stylistic.

**Surface kinds must be untouched.** Today a `Surface` kind's arithmetic is
"same field, same reading", with `availability` exactly `1.0` — "an IEEE-754
no-op (verified over the roster's real values, bit-difference 0)". Preserve
that property and verify it the same way it was verified before: in bits.

**Verified against the tree at `3d074d2af`, so you need not re-derive it:**

- The two `(CHEMOSYNTHATE, 0.0)` placeholders Task 7 left are at
  `lib.rs:1571` and `lib.rs:1863`. Those are the two lines that stop being
  zero.
- The fields you consume: `subterranean_energy_field_per_rung`
  (`windows/worldgen/src/energy.rs:510`),
  `subterranean_substrate_field_per_rung` (`lib.rs:3060`),
  `subterranean_substrate_at_rung` (`lib.rs:3024`).
- **`rung_evaluation_depth_m` is still NOT re-exported** at
  `domains/terrain/src/lib.rs` — Task 3's deferred minor, confirmed still
  open. It is the only `pub fn` in `delve.rs` missing from that crate root's
  `pub use delve::{...}` list. **Add it here**, since you are the first task
  that would otherwise be forced into the fully-qualified path. That is the
  smallest change that closes a real inconsistency, and it was deferred to
  this task deliberately.

**A disclosed asymmetry you must NOT silently inherit.** Task 8's review
traced `is_ametabolic`'s call sites and found that
`windows/vessel/src/liveness.rs:3897-3907` and `:4431` gate **all homeostatic
drives, including hunger, on `is_ametabolic(thermal)` alone** — that gate
never inspects `niche` or `trophic_mode`. So after you give `xorn` a
`CHEMOSYNTHATE` weight, it will **consume chemosynthate in the capacity model
and still feel no hunger in the agent model**. That is pre-existing, it is
not yours to fix, and it is out of scope. **State it in your report** so it
lands in the campaign's follow-up register rather than being discovered by
whoever next wonders why a rock-eater never eats.

- [ ] **Step 1: Capture the "before" values**

Before changing anything, capture per-species suitability for every kind on
the frozen seed set. Read
`windows/worldgen/tests/suite/underworld_separation.rs` and
`clients/game/core/tests/fixtures/` for the fixture pattern this repo already
uses, and **follow it rather than inventing one**.

- [ ] **Step 2: Write the tests that pin what must NOT move**

```rust
/// The per-rung switch touches only the `Subterranean` arm. Asserted in
/// BITS, not approximately, because approximately is how a systematic drift
/// hides -- and because the shipped comment this preserves made its own
/// claim in bits ("an IEEE-754 no-op, bit-difference 0").
#[test]
#[ignore = "heavy: live-worldgen battery over the frozen seed set"]
fn exactly_the_subterranean_roster_moves() {
    let before = load_captured_suitability();  // Step 1's fixture
    let (geo, terrain, climate, ..) = world_at(42);
    let now = per_species_suitability(/* the shipped call, as the tree spells it */);

    let subterranean: BTreeSet<&str> = ["rust-monster", "xorn", "drow"].into_iter().collect();
    let mut moved = BTreeSet::new();
    let mut compared = 0usize;

    for (kind, map) in now.iter_by_kind() {
        for vertex in geo.vertices() {
            let a = before.at(kind, vertex).to_bits();
            let b = map.at(vertex).to_bits();
            if a != b {
                moved.insert(kind);
            }
            compared += 1;
        }
    }

    assert!(compared > 10_000, "only {compared} samples compared — vacuous");

    // DIRECTION 1: nothing outside the roster moved.
    let strays: Vec<_> = moved.difference(&subterranean).collect();
    assert!(
        strays.is_empty(),
        "{strays:?} moved and are not Subterranean — the per-rung switch \
         leaked out of the Subterranean arm"
    );

    // DIRECTION 2: something INSIDE the roster did. A test with only
    // direction 1 passes trivially when the switch is wired to nothing,
    // which is the failure this campaign is most likely to ship green.
    assert!(
        !moved.is_empty(),
        "no kind moved at all — the per-rung field and the CHEMOSYNTHATE \
         supply are not reaching the capacity loop"
    );
}
```

**Both directions are load-bearing and the second is the one that would
otherwise be missing.** A test asserting only "nothing else moved" passes
perfectly when the switch does nothing at all — and this repo has shipped
that exact shape before (`underworld_conditions_probe.rs:129`: neutralising
the depth left all 614 worldgen tests green).

`iter_by_kind`, `at`, and the real spelling of the `per_species_suitability`
call are **placeholders for whatever the tree actually provides** — read
`windows/worldgen/tests/suite/underworld_separation.rs`, which already
iterates this structure, and use its spelling.

- [ ] **Step 3: Make the switch — including xorn's niche**

Three things land together, and together is the point (Ruling P2):

1. the `Subterranean` arm reads the best rung of the per-rung fields;
2. `CHEMOSYNTHATE`'s entry in both `per_axis` arrays stops being `0.0` and
   reads `subterranean_energy_field_per_rung`;
3. **xorn's `niche` gains its `CHEMOSYNTHATE` weight** (deferred here from
   Task 8). Choose the split against `MINERAL` deliberately and justify it in
   the row's comment: mineral is what a xorn is *made of*, chemosynthate is
   what *powers* it.

Landing (3) without (2) makes xorn eat a field of zeros; landing (2) without
(3) means the new axis has no consumer. Either alone is a number moved for a
reason nobody would be able to attribute later.

- [ ] **Step 4: Measure what moved — the whole point of the task**

```bash
cargo nextest run --workspace --no-fail-fast > /tmp/hv-t9.log 2>&1; echo "exit=$?"
grep -E '^ *Summary|FAILED|panicked' /tmp/hv-t9.log
make rebaseline
git diff --stat -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$')
```

| what you observe | what to do |
|---|---|
| only `docs/audits/` and the three subterranean kinds' derived values moved | expected; commit the regenerated artifacts in the same commit |
| a surface kind moved | **STOP.** The switch leaked out of the `Subterranean` arm |
| nothing moved at all | **STOP.** The switch is not wired to anything |
| `book/src/gallery/` moved | **STOP** — that is an epoch event, not a value change |
| a byte-golden fixture moved | inspect it before accepting. `make rebaseline-goldens` accepts drift and must **never** be run on an unexplained diff |

- [ ] **Step 5: The census question — measure, then STOP**

```bash
cargo nextest run -p hornvale-lab --run-ignored all -E 'test(census_sentinel)'
```

**NOT `make lab-diff STUDY=the-census` — it is VACUOUS locally, and Task 9
found this the hard way.** That target compares
`git show HEAD:book/src/laboratory/generated/<study>/rows.csv` against the
**working-tree** copy of the same file. Nothing regenerates the working-tree
copy without a census run, and a census run is host-guarded to lefford. So
both sides are the same bytes and it reports "No metric moved" whatever you
changed. It is a real check *after* a refresh and a null generator before
one.

`census_sentinel` is the live instrument: it re-probes a small seed set
against committed fixtures, so it actually executes the code you changed.
Read `windows/lab/tests/suite.rs` and
`windows/lab/tests/fixtures/sentinel-waivers.txt` before interpreting a
result.

| what you observe | what to do |
|---|---|
| no metric moved | record the command and its output verbatim; the campaign proceeds |
| any metric moved | **Report it in full and CONTINUE.** Nathan pre-authorized the regen on 2026-08-26. Report the verbatim output, which metrics moved and by how much. You still do NOT run the regen — it is the controller's job, on the canonical box, behind a host guard, and `HV_CENSUS=1` is never yours to set |

The spec is explicit that the census's named capacity columns being
non-subterranean is **not** sufficient to conclude it does not move. This
command is the instrument; the reasoning is not.

**A census refresh costs about fifteen minutes, not hours.** The timings
ledger's last eight runs cluster at **855–918 s** on lefford. An earlier
draft of this plan said "hours", which was a stale figure lifted from
`CLAUDE.md`'s prose — a 19,207 s run whose successor campaign indexed the
slow query away, taking the next run to 949 s. `CLAUDE.md` warns against
reading a cost off that block and uses this exact error as its worked
example. Read `grep '| census |' docs/timings.md | tail` instead; it is the
ledger the prose points at and it moves far faster than the prose does.

Why the correction is in the plan rather than only in a ledger: a check
described as guarding something hours long invites hoping the answer is
"nothing moved".

- [ ] **Step 6: Discharge `MAP-per-rung-substrate`**

Rewrite the row: what shipped, what it measured, and the `Nadir` control.
Move its status off `raw`. Then:

```bash
cargo nextest run -p hornvale --test suite -E 'test(registry_ids_are_unique)'
```

- [ ] **Step 7: Commit and push**

---

### Task 10: The hydrothermal vent unblock

**Files:**
- Modify: `windows/worldgen/src/lib.rs` (`marine_forage_supply_field`, ~1021)

**Interfaces:**
- Consumes: Task 4's `EnergySource`.
- Produces: a vent that is productive for a stated reason.

`marine_forage_supply_field` gives `Biome::HydrothermalVent` a productivity
of `0.02`, and its comment is its own repeal condition:

> Chemotrophic in reality; not modellable as forage yet (BIO-chemotrophy).

That refusal is now false. Metaplan §4 calls this "free evidence that the
mechanism is not underworld-special-cased" — **so the vent's productivity
must come from the same source functions a chamber's does**, not from a new
literal. A hand-tuned constant here would defeat the entire purpose of doing
this in this rung rather than a later one.

**What the controller verified about vents, because it makes this easier than
the brief implies** (at `eeb120ba7`):

A vent is not a special case bolted onto the sea — it is
`SeafloorFeature::Ridge` (`domains/climate/src/biome.rs:712-714`), i.e. a
mid-ocean ridge. That matters three ways, and all three are free:

1. `geothermal_gradient_at` reads `crust_age_at`
   (`domains/terrain/src/provider.rs:549-555`), and **ridge crust is the
   youngest there is**, so the gradient at a vent is at its maximum. The
   `Geothermal` source needs no special-casing to be strong there.
2. Ridge rock is **mafic to ultramafic** — low silica — which is exactly
   `Serpentinization`'s and `IronReduction`'s band. Serpentinization at
   mid-ocean ridges is the real mechanism (the Lost City field), so the
   sources you already have are the *right* sources for a vent.
3. Moisture at a vent is saturation by definition. Drainage is a land
   quantity and does not apply offshore — decide what to pass and say why.

So metaplan §4's claim that this is "free evidence the mechanism is not
underworld-special-cased" is stronger than it looks: the seven sources land
on a vent correctly **because a vent is young mafic rock with a hot
gradient**, which is what two of them describe.

**THE ONE REAL DESIGN QUESTION, which the controller is NOT deciding.**
`Geothermal`'s yield rises with ΔT, and ΔT is `gradient × depth`. At the
seafloor interface depth is ~0, so ΔT is ~0 and `Geothermal` yields almost
nothing — which is wrong for a vent, whose fluid is hot *precisely because it
circulated deep and came back up*.

So a vent cannot simply be evaluated at depth 0. **Name the property you are
modelling — energy delivered to the interface from a circulation depth — and
choose the depth accordingly, with a one-line physical justification in the
doc and a citation if one exists.** Do not invent a constant without saying
it is un-anchored; Task 4 spent two fix rounds learning that an un-anchored
constant is not merely undocumented but unfalsifiable.

- [ ] **Step 1: Write the failing test**

```rust
/// Vents outproduce the abyssal plain they sit on, AND their productivity
/// VARIES between vents -- because it derives from that vent's own rock and
/// gradient. Both halves are needed and neither alone is sufficient:
/// "productive" is satisfied by simply raising 0.02 to 0.6, and "varies" is
/// satisfied by noise. Together they say a mechanism arrived.
#[test]
#[ignore = "heavy: live-worldgen battery"]
fn a_vent_is_productive_and_not_by_a_literal() {
    let (geo, terrain, climate) = world_at(42);
    let biome = climate.biome_map();
    let field = marine_forage_supply_field(&geo, &terrain, &climate, 1.0);

    let mut vents: Vec<f64> = Vec::new();
    let mut abyssal: Vec<f64> = Vec::new();
    for vertex in geo.vertices() {
        match biome.get(vertex) {
            Biome::HydrothermalVent => vents.push(*field.get(vertex)),
            Biome::Abyssal => abyssal.push(*field.get(vertex)),
            _ => {}
        }
    }
    assert!(vents.len() >= 20, "only {} vents — vacuous", vents.len());
    assert!(!abyssal.is_empty(), "no abyssal plain to compare against");

    let median = |mut v: Vec<f64>| {
        v.sort_by(f64::total_cmp);
        v[v.len() / 2]
    };
    let vent_median = median(vents.clone());
    assert!(
        vent_median > median(abyssal),
        "vents ({vent_median}) do not outproduce the abyssal plain they sit on"
    );

    let mut distinct = vents.clone();
    distinct.sort_by(f64::total_cmp);
    distinct.dedup_by(|a, b| a.to_bits() == b.to_bits());
    assert!(
        distinct.len() > 1,
        "every one of {} vents reports the identical productivity — the \
         mechanism did not arrive, a different constant did",
        vents.len()
    );
}
```

The vent-count floor comes first deliberately: without it this test passes
perfectly on a world that happens to have no vents at all, which is the
vacuous-green shape this repo keeps finding.

- [ ] **Step 2: Implement**

- [ ] **Step 3: Measure the movement**

This moves marine numbers. Apply Task 9's Step 4 branch table again, and its
Step 5 census check again — **a vent productivity change is considerably more
likely to reach a census metric than a subterranean one**, because marine
biomes are not a three-kind roster.

- [ ] **Step 4: Commit and push**

---

### Task 11: Definition of Done

- [x] **Step 1: The metaplan**

`docs/superpowers/specs/2026-08-24-the-underworld-larder-metaplan.md`: §3.1
and §3.2 carry Task 1's re-run figures. §7 is marked measured, with Task 6's
number. Rung 2's own section is marked done with a pointer to the chronicle.

Done: §3.1/§3.2 each carry a "Re-run 2026-08-26" confirmation paragraph
(bit-for-bit reproduction); §7's heading now reads "MEASURED (The Sources)"
with the 0.145 result and its consequence for rung 3; rung 2's own section
heading reads "— DONE" with a paragraph pointing at
`book/src/chronicle/the-sources.md` and correcting its own "free evidence"
framing; rung 3's section gained an "Inherited diagnosis" paragraph naming
the ~3-state lithology finding so it does not design against magnitude
lithology cannot carry.

- [x] **Step 2: The idea registry**

- `MAP-per-rung-substrate` discharged (Task 9).
- `BIO-underworld-has-no-energy` carrying the corrected size clause.
- `BIO-subterranean-energy-sources` landed and moved off `raw`.
- **`BIO-chemotrophy` untouched — it stays `raw`, and the status is Nathan's
  call.** Do not move it even though this rung is exactly what its promise
  named; its promise spans rungs 2 and 3.
- `rust-monster`'s open trophic question recorded — it is
  `Ectothermic`/`Heterotrophic` with the same pure-`MINERAL` niche and the
  comment "walks the surface eating metal", so by Task 8's own argument it is
  arguably chemotrophic too. Not decided by this campaign.

Done: `book/src/frontier/idea-registry.md` — `BIO-subterranean-energy-sources`
flipped `raw` → `shipped`, prose replaced (not appended, per this
directory's own discipline against narrative accretion) to state what
shipped, Where repointed at the chronicle and `energy.rs`.
`MAP-per-rung-substrate`'s Where cell was pointing at the wrong chronicle
(`the-underworld.md`, an unrelated earlier campaign) — corrected to
`the-sources.md`. `BIO-underworld-has-no-energy` verified: already carries
the corrected size clause, untouched. `BIO-chemotrophy` verified untouched,
still `raw`. `rust-monster`'s open question is already recorded in the tree
it belongs to, `domains/species/src/lib.rs:3480` (the "rust-monster shares
the pure-MINERAL niche... it walks the surface eating metal" comment landed
by Task 9) — confirmed present, not duplicated into the registry (which
this directory's own guidance treats as narrative accretion, not a home for
an open per-kind question already recorded beside the code it's about).

- [x] **Step 3: The book**

A chronicle entry `book/src/chronicle/the-sources.md`. A freshness sweep of
stale chapters — at minimum the underworld and community-axes chapters, which
now have a *derived* counterpart to their *authored* corpus, and the chapter
that describes carrying capacity as insolation-fed. Re-score
`book/src/open-questions.md` if this campaign moved one of the Confidence
Gradient's bets (decision 0030).

Done: `book/src/chronicle/the-sources.md` written and wired into
`book/src/SUMMARY.md`. Swept `book/src/domains/species.md` (the chapter that
actually carries the subterranean habitat-realm component and the resource
niche this campaign touches): fixed a stale field list
(`metabolic class` → `thermal strategy, trophic mode`), corrected "two
today" to "three today" (xorn, rust monster, drow — drow predates this
campaign and was simply never updated), and added a paragraph on per-rung
resolution and the new energy term. Checked `book/src/domains/climate.md`
for the underworld's authored corpus and found no book chapter describes it
at all outside the frontier registry — nothing there to correct. Re-scored
`book/src/open-questions.md` near line 1127 (the Mountain-dwarf/Duergar
depth-distinction bet): moved, not closed.

- [x] **Step 4: The retrospective**

`docs/retrospectives/the-sources.md` — process lessons, not product. Write it
**before** the merge submission, not after. Add a one-line entry to
`docs/retrospectives/README.md`'s grouped index.

Done: both written.

- [ ] **Step 5: Artifacts, then the merge**

```bash
make rebaseline
git diff --exit-code -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$')
make gate-commit
```

Then `submitting-to-the-sluice`. The merge message needs a
`Sluice-Headline:` trailer in the final trailer block, adjacent to
`Claude-Session:` with **no blank line between them** — verify the exact
requirement against `scripts/sluice-headline.sh`, because the skill's own
prose about this is stale and the scripts refuse without it.

## Notes for the executor

- **Verify each task's brief against the tree immediately before starting
  it**, not at plan-authoring time. This is the single highest-yield habit
  from The Gossan: two of six defects were caught this way, both structural,
  both invisible from inside the task that would have hit them. Every line
  number in this plan was correct at `7576eca00` and every one of them rots.
- **Absorb `main` at every stage boundary** with
  `make sluice-stage BRANCH=campaign/the-sources REF=<full-sha>`, which gates
  the real merge product without pushing it. A conflict is refused at the
  mouth in milliseconds — that is the signal to absorb locally and resubmit.
- **`make gate-commit` costs what it rebuilds.** Task 7 is the kernel-layer
  edit and the expensive one; `domains/`- and `windows/`-layer edits are far
  cheaper.
- **Run once, inspect many.** Redirect to a log, then grep the file. Never
  re-run a heavy suite to read a second line — this workspace's suite time is
  dominated by test runtime, so a second run costs the whole thing again.
- **Post to the board** (`make board-post KIND=technique NOTE='…'`) whenever
  you learn an operational fact the hard way. Name identifiers **bare** in
  that NOTE — backticks execute in shell-interpolated text.
