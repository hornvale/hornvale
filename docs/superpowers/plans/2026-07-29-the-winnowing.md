# The Winnowing Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Let a caller say which per-tile layers it wants, so the tiles document stops carrying 9.25 MB the client parses and discards — without moving a single byte of any field it does emit.

**Architecture:** A `TileFields` name-set plus a projected serializer in `windows/scene`. `scene_json` keeps its existing `serde_json::to_string(scene)` derive path untouched, so the default document is byte-identical *by construction*; `scene_json_selected` uses a manual `Serialize` impl that skips unselected arrays. A permanent equality test between the two is the drift guard. The catalog gains one export.

**Tech Stack:** Rust 2024, workspace crates only. No new dependencies.

## Global Constraints

- **No new dependencies** — `serde` + `serde_json` only (`cli/tests/architecture.rs`).
- **No `HashMap`/`HashSet`** — `BTreeMap`/`BTreeSet`/`Vec` only (`clippy.toml`). `TileFields` therefore holds a `BTreeSet<&'static str>` or a `Vec<bool>`, never a hash set.
- **No wall-clock in the sim**; scoped `#[allow(clippy::disallowed_types)]` with a comment at any `Instant` site.
- **`#![warn(missing_docs)]`** — every public item, field and variant. `TileFields` is new public API and the type audit is default-deny; `TilesScene`'s existing `type-audit:` tag block (`windows/scene/src/lib.rs:125`) is the model for tagging.
- **Byte-identity is constitutional.** The default document must not move. Verified by construction (§Task 1 Step 4) *and* by checksum.
- **`cargo fmt`** as the final step before every commit.
- **Seed 42**, width 512, is the reference document.
- **Verified in-tree, do not guess:**
  - `scene_json` is exactly `serde_json::to_string(scene).expect("a TilesScene always serializes")` (`windows/scene/src/lib.rs`).
  - `TilesScene` derives `#[derive(Debug, Serialize)]` (`:126`) and is serialized **nowhere else in the workspace** (grepped: no other `TilesScene` reference outside `windows/scene/src/lib.rs`).
  - Float arrays carry `#[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::vec_f64_field")]`; the scalar uses `::f64_field`.
  - The catalog's input-buffer idiom is `hw_new_pinned` (`clients/world-wasm/src/lib.rs:157`), reading `len` bytes of JSON from `INBUF`.

---

### Task 1: `TileFields` and the projected serializer

**Files:**
- Modify: `windows/scene/src/lib.rs`

**Interfaces:**
- Produces, for Task 2: `hornvale_scene::TileFields`, `TileFields::all()`, `TileFields::parse_json(&str) -> Result<TileFields, SceneError>`, `scene_json_selected(&TilesScene, &TileFields) -> String`.

- [ ] **Step 1: Capture the byte baseline**

```bash
mkdir -p /tmp/winnowing-baseline
```
Emit the seed-42 width-512 tiles document to `/tmp/winnowing-baseline/tiles-512.json` (a scratch example you delete before committing; `windows/scene/examples/profile_scene.rs` shows the call shape), then `sha256sum` it into `SUMS`. Paste `SUMS` in your report.

- [ ] **Step 2: Write the failing tests first**

Two tests, in `windows/scene/src/lib.rs`'s test module. Neither compiles yet.

```rust
/// The drift guard: the projected serializer at `all()` must reproduce the
/// derive byte for byte. If someone adds a field to `TilesScene` and forgets
/// the manual impl, this reds immediately.
#[test]
fn the_full_projection_equals_the_derive() {
    let world = mooned_world();
    let scene = tiles_scene(&world, 64).expect("tiles");
    assert_eq!(scene_json(&scene), scene_json_selected(&scene, &TileFields::all()));
}

/// The design's load-bearing property: a field's bytes do not depend on which
/// other fields were requested. Assert it for every layer, so the golden story
/// is nineteen assertions instead of 2^19 documents.
#[test]
fn each_field_serializes_independently_of_the_others() {
    let world = mooned_world();
    let scene = tiles_scene(&world, 64).expect("tiles");
    let full = scene_json(&scene);
    for name in TileFields::ALL_NAMES {
        let one = scene_json_selected(&scene, &TileFields::only(&[name]).expect("known field"));
        let fragment = field_fragment(&one, name);
        assert!(
            full.contains(&fragment),
            "field {name} serializes differently alone than in the full document"
        );
    }
}
```

`field_fragment` is a test helper you write: given a document and a field name, return the exact `"name":[...]` substring, by locating `"\"{name}\":"` and taking the balanced `[...]` (or the scalar run) after it. Keep it in the test module.

`mooned_world()` is the existing helper in `lib.rs`'s test module (it is `gen_world_for(42)`, the same world `region.rs`'s `gen42()` builds). Width 64 keeps the test inside the commit gate.

- [ ] **Step 3: Run them and watch them fail to compile**

Run: `cargo test -p hornvale-scene the_full_projection_equals_the_derive`
Expected: compile error — `TileFields` and `scene_json_selected` do not exist.

- [ ] **Step 4: Add `TileFields`**

```rust
/// The per-tile layers a caller wants in a `scene/tiles/v1` document.
///
/// The document's nineteen per-tile arrays are 99.8% of its bytes, and a
/// given client typically reads a subset — the Orrery reads ten. This is
/// `BuildDepth`'s "only as deep as the question asks" at the emit boundary:
/// the sim still computes every layer, the caller chooses what crosses the
/// wire (decision 0022).
///
/// Field names are the wire's own names. An unknown name is an error, never
/// a silently-dropped layer.
/// type-audit: bare-ok(identifier-text: field names)
#[derive(Debug, Clone)]
pub struct TileFields {
    /// One flag per name in [`TileFields::ALL_NAMES`], same order.
    selected: Vec<bool>,
}
```

`ALL_NAMES: &[&str]` lists the nineteen per-tile array names **in the struct's own field order**: `elevation_m`, `ocean`, `biome`, `plate`, `unrest`, `t_mean_c`, `t_swing_c`, `t_diurnal_amp_c`, `moisture`, `current_east`, `current_north`, `precip_mm_yr`, `snow_fraction`, `precip_regime`, `cloud_fraction`, `weather_propensity`, `cloud_type`, `water`, `drainage`.

**Read the struct and confirm that list against it before writing it down** — the order and the exact spelling are the contract, and this plan's list was transcribed by eye.

Provide: `all()`, `only(&[&str]) -> Result<TileFields, SceneError>` (unknown name → error naming the offender and listing the valid names), `contains(&self, name: &str) -> bool`, and `parse_json(&str) -> Result<TileFields, SceneError>` accepting a JSON array of strings.

Add a `SceneError::UnknownTileField(String)` variant with a `Display` arm matching the file's existing style.

- [ ] **Step 5: Add the projected serializer**

A private wrapper with a **manual** `Serialize` impl:

```rust
struct Projected<'a> {
    scene: &'a TilesScene,
    fields: &'a TileFields,
}
```

It emits `serialize_map(None)`, writing every key **in `TilesScene`'s declaration order**, skipping a per-tile array when `!fields.contains(name)`. Always-emitted metadata: `schema`, `seed`, `width`, `height`, `sea_level_m`, `season_period_days`, `locked`, `circulation_bands`, `biome_legend`, `water_legend`, `features`, `waterfalls`.

To reuse the quantizing serializers inside a manual impl, wrap them in newtypes:

```rust
/// Serializes a `Vec<f64>` through the kernel's quantizer, so a projected
/// document's floats are byte-identical to the derive's.
struct QVec<'a>(&'a Vec<f64>);
impl serde::Serialize for QVec<'_> {
    fn serialize<S: serde::Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
        hornvale_kernel::quantize::quantize_serde::vec_f64_field(self.0, s)
    }
}
```
and the same shape for the scalar `f64_field`. **Every float field must go through these** — a raw `serialize_value(&self.scene.elevation_m)` would emit unquantized floats and move bytes.

Then:

```rust
/// Serialize a tiles document carrying only `fields`' per-tile layers.
/// Document metadata is always present. A field that IS emitted is
/// byte-identical to [`scene_json`]'s output for it.
pub fn scene_json_selected(scene: &TilesScene, fields: &TileFields) -> String {
    serde_json::to_string(&Projected { scene, fields }).expect("a TilesScene always serializes")
}
```

**Leave `scene_json` exactly as it is.** It keeps using the derive, so the default document is byte-identical by construction rather than by assertion — and Step 2's equality test is then a real, independent check of the manual impl rather than a tautology.

- [ ] **Step 6: Run both tests**

Run: `cargo test -p hornvale-scene`
Expected: PASS, including the two new tests and the existing goldens.

If `the_full_projection_equals_the_derive` fails, diff the two strings and report the first divergence — that is a field you missed, mis-ordered, or serialized without its quantizer. Do not "fix" it by changing `scene_json`.

- [ ] **Step 7: Re-verify the baseline and commit**

```bash
sha256sum -c /tmp/winnowing-baseline/SUMS
cargo fmt && cargo clippy --workspace --all-targets -- -D warnings
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
```
All must pass. Delete the scratch emitter. Commit.

---

### Task 2: The catalog export

**Files:**
- Modify: `clients/world-wasm/src/lib.rs`

- [ ] **Step 1: Add the export**

```rust
/// Emit `scene/tiles/v1` carrying only the per-tile layers named by a JSON
/// array of `len` bytes in the input buffer. 0 ok; 2 scene error; -1 len
/// exceeds the buffer; -2 not UTF-8; -3 no world live or bad field list.
#[unsafe(no_mangle)]
pub extern "C" fn hw_scene_tiles_selected(width: u32, len: usize) -> i32 { … }
```

Follow `hw_new_pinned`'s input-buffer handling (`:157`) for the `-1`/`-2` paths and `hw_scene_tiles` (`:244`) for the world/scene handling. Reuse the `SceneContext` the catalog already holds — call `tiles_scene_in`, not `tiles_scene`.

`hw_scene_tiles(width)` is unchanged.

- [ ] **Step 2: Extend the discipline guard**

`cli/tests/scene_context_discipline.rs` asserts every terrain-facing export reaches `scene_ctx(`. Add the new export to its `TERRAIN_EXPORTS` table so it is covered too.

**Verify by mutation:** point the new export at `SceneContext::build(world)` instead of `scene_ctx(world)`, confirm the test REDS, restore, confirm GREEN. Paste both.

- [ ] **Step 3: Drive it end to end**

Extend `clients/world-wasm/drive.mjs` with a check that a projected document (a) parses, (b) omits an unrequested field, (c) contains a requested field whose bytes match the full document's for that field.

Run: `make world-check`. Report the wasm size delta, **measured at a constant build path** — the binary embeds `#[track_caller]` panic locations, so path length changes the number and comparing across different paths measures the path.

- [ ] **Step 4: fmt, clippy, commit**

---

### Task 3: Measure and close

**Files:**
- Modify: `windows/scene/examples/profile_scene.rs`
- Create: `book/src/chronicle/the-winnowing.md`, `docs/retrospectives/the-winnowing.md`
- Modify: `book/src/SUMMARY.md`, `docs/timings.md`

- [ ] **Step 1: Measure**

Extend `profile_scene.rs` to emit both the full document and the Orrery's ten-field projection (`elevation_m`, `ocean`, `biome`, `plate`, `unrest`, `t_mean_c`, `t_swing_c`, `moisture`, `water`, plus `features` which is always emitted — **confirm this list against `orrery src/sim/scene.ts`'s `parseTiles` rather than trusting the plan**), printing bytes and serialize milliseconds for each.

Spec §5's hypothesis: 17.73 MB → ~8.48 MB, serialize ~553 → ~265 ms. **Both time figures assume cost is proportional to bytes, which is an assumption.** If serialize does not fall roughly in proportion, that is the finding — report it prominently rather than writing down the smaller number.

- [ ] **Step 2: Ledger the run**

```bash
bash scripts/timed.sh scene-profile -- cargo run --release -p hornvale-scene --example profile_scene -- 8
```
(`--` separator required, `scripts/timed.sh:16-17`.)

- [ ] **Step 3: Gates and drift**

`make gate`, then `make gate-full`, then `make rebaseline` + `git diff --exit-code book/src/gallery/ book/src/reference/ book/src/laboratory/ docs/audits/`. Report any inherited red with its test name; do not fix it.

- [ ] **Step 4: Chronicle, retro, freshness sweep**

Chronicle at `book/src/chronicle/the-winnowing.md`, into `SUMMARY.md` after The Cistern (the list is merge-chronological). Prose altitude: technical and mathematical (root `CLAUDE.md`).

Carry the measured composition table, the 52.2% finding, and — this matters — **the distinction between the measured byte reduction and the inferred parse reduction** (spec §4). The node figure is a proxy; the browser measurement lives in another repo's harness.

Freshness sweep: `grep -rn "17.3 MB\|17,728,743\|tiles document\|scene/tiles" book/src/ --include=*.md`. The Cistern's chronicle names the JSON size as the next lever — it should now point at this campaign. Re-score `book/src/open-questions.md` if a bet moved (decision 0030); flip any idea-registry row.

Retrospective — process lessons, not product (decision 0020).

- [ ] **Step 5: Commit**

---

## Self-review

**Spec coverage.** §3.1 `TileFields` → Task 1 Step 4. §3.2 emit-boundary projection → Task 1 Step 5. §3.3 catalog export → Task 2. §3.4 the golden story → Task 1 Step 2 (both tests) plus the existing full golden, which Step 7's checksum re-verifies. §4 verification → distributed. §5's expected result → Task 3 Step 1, framed as a hypothesis. §7 route A → this whole plan; route B is out of scope and named in the spec.

**Placeholders.** None. Task 3 Step 1's field list and Task 1 Step 4's name list both carry an explicit instruction to verify against the source rather than trust the plan — they were transcribed by eye and that is stated.

**Type consistency.** `scene_json_selected(&TilesScene, &TileFields) -> String` is used identically in Tasks 1, 2 and 3. `TileFields::only(&[&str]) -> Result<TileFields, SceneError>` and `parse_json(&str) -> Result<TileFields, SceneError>` share the error type with the rest of the crate.

**Known risk.** The manual `Serialize` impl duplicates `TilesScene`'s field order and its `serialize_with` choices, so a field added to the struct and not to the impl would silently vanish from every projected document. That is exactly what `the_full_projection_equals_the_derive` catches, which is why `scene_json` must keep using the derive — if both paths went through the manual impl the guard would compare a thing to itself and prove nothing. This is the same tautology trap The Cistern's equivalence test fell into; it is avoided here deliberately.
