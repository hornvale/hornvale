# The Benchmark Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Give height-above-sea-level its own kernel type so a band or display
function cannot be handed a raw isostatic reading, and correct the three
surfaces that are handed one today.

**Architecture:** A new kernel newtype `SeaLevelHeight`, produced by the named
conversion `ReferenceElevation::above(datum)` (decision 0008's "validating
constructors and *named conversions*"). Consuming functions type their
*parameters* with it, which turns the live bug into a compile error. Field names
carry the datum so the discipline survives serialization, where types cannot go.
`scene/surrounds` is minted `v2` because every observable relief band value
changes.

**Revised in flight** (Task 2): the original design retyped `Sub`'s output
instead. The compiler produced 21 errors including orographic-rise sites that
are not sea-level quantities at all, proving elevation subtraction is
polymorphic in meaning. See spec §4.1's post-G3 amendment.

**Tech Stack:** Rust edition 2024, no new dependencies (`serde`, `serde_json`,
`libm` only), `cargo nextest`, `mdbook`.

Spec: `docs/superpowers/specs/2026-08-06-the-benchmark-design.md`.

## Global Constraints

- **No new dependencies.** The allowlist is `ALLOWED_EXTERNAL` in `cli/tests/architecture.rs`.
- **No `HashMap`/`HashSet`** — `BTreeMap`/`BTreeSet`/`Vec` only; enforced by `clippy.toml` `disallowed-types` workspace-wide, tests included.
- **No wall-clock time.** `std::time::Instant` is banned in test code too.
- **Quantize at emit only** — never in the compute path (`hornvale_kernel::quantize`).
- **Every crate sets `#![warn(missing_docs)]`** — every new pub item, field and variant gets a one-line doc comment.
- **Every primitive at a `pub` boundary carries a `type-audit:` verdict tag.**
- **`docs/audits/type-audit-report.md` is regenerated in the same commit as any pub-boundary change.** The pre-commit hook runs `make quick`, whose report-freshness half fails in ~9 s otherwise. This is a required mechanical regen, not a scope violation — include it and never reach for `--no-verify`:
  `cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md`
- **`cargo fmt` as the final step before every commit.** Fmt-gate skips are the most common review finding.
- **Layering:** `kernel/` → `domains/*` → `windows/*` → `cli/`. A domain depends on `hornvale-kernel` and nothing else.
- **The verbatim heavy-tier ignore reason**, if any test needs one, is exactly:
  `heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full`
- **Determinism:** the seed-42 world must stay byte-identical. `sea-level-m` and `highest-elevation-m` are committed facts and must not change.

---

## File Structure

| file | responsibility | task |
|---|---|---|
| `kernel/src/units.rs` | `SeaLevelHeight`; `ReferenceElevation::above` | 1, 2 |
| `windows/locale/src/lib.rs` | `LocaleFields.height_asl_m` + its serializer | 3 |
| `windows/scene/src/surrounds.rs` | `relief_band` parameter type, `v2` schema, `sea_level_m`, `height_asl_m` | 4 |
| `book/src/reference/scene-surrounds-v1.md` | → `scene-surrounds-v2.md` reference page | 4 |
| `windows/vessel/src/focalize.rs` | the biome datum's reported height | 5 |
| `cli/src/main.rs` | the `locale` renderer's height line | 5 |
| `windows/worldgen/src/lib.rs` | rename the field holding a difference | 6 |
| `docs/design/kernel-units-doctrine.md` | the `elevation-convention` waiver's status | 6 |

---

### Task 1: The `SeaLevelHeight` kernel type

Additive only — `Sub`'s output is unchanged in this task, so the workspace stays
green and this task is reviewable on its own.

**Files:**
- Modify: `kernel/src/units.rs` (insert after `ReferenceElevation`'s `impl`, before the existing `impl Sub for ReferenceElevation` at :89)
- Test: `kernel/src/units.rs` (the existing `#[cfg(test)] mod tests`)

**Interfaces:**
- Consumes: `ReferenceElevation` (existing), `std::cmp::Ordering` (already imported)
- Produces: `SeaLevelHeight` with `from_metres(f64) -> Self`, `get(self) -> f64`, `depth(self) -> f64`, `total_cmp(self, Self) -> Ordering`

- [ ] **Step 1: Write the failing tests**

Add to `kernel/src/units.rs`'s test module:

```rust
#[test]
fn a_sea_level_height_reports_its_metres() {
    let h = SeaLevelHeight::from_metres(1200.5);
    assert!((h.get() - 1200.5).abs() < 1e-12);
}

#[test]
fn depth_is_the_positive_downward_reading() {
    let below = SeaLevelHeight::from_metres(-3000.0);
    assert!((below.depth() - 3000.0).abs() < 1e-12, "depth reads positive downward");
    assert!((below.depth() + below.get()).abs() < 1e-12, "depth is exactly -height");
    let above = SeaLevelHeight::from_metres(800.0);
    assert!(above.depth() < 0.0, "above sea level, depth is negative");
}

#[test]
fn heights_order_deterministically() {
    let a = SeaLevelHeight::from_metres(-10.0);
    let b = SeaLevelHeight::from_metres(10.0);
    assert_eq!(a.total_cmp(b), std::cmp::Ordering::Less);
    assert_eq!(b.total_cmp(a), std::cmp::Ordering::Greater);
    assert_eq!(a.total_cmp(a), std::cmp::Ordering::Equal);
}
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `cargo test -p hornvale-kernel --lib units`
Expected: FAIL to compile — `cannot find type SeaLevelHeight in this scope`.

- [ ] **Step 3: Write the type**

Insert into `kernel/src/units.rs` immediately before the existing
`impl Sub for ReferenceElevation`:

```rust
/// Metres above this world's sea level. Signed: negative below.
///
/// Distinguished at the type level from [`ReferenceElevation`], which is an
/// absolute reading on the planet-independent isostatic datum. A
/// `SeaLevelHeight` is *per-world* — its zero is a derived value of that other
/// type — so two of these from different worlds are comparable to each other in
/// a way their `ReferenceElevation`s are not, and vice versa. Decision 0044's
/// doctrine requires an interval type to carry its datum; this type's name is
/// that datum.
///
/// Produced by subtracting two [`ReferenceElevation`]s (see
/// [`Sub`](std::ops::Sub) for that type), or via
/// [`from_metres`](Self::from_metres) for a caller with no pair to subtract.
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub struct SeaLevelHeight(f64);

impl SeaLevelHeight {
    /// Builds a height directly from metres rather than from a difference of
    /// two readings. It exists for one reason: a caller deserializing a
    /// document has a number, not the pair of elevations it came from.
    ///
    /// **This is the hole through which the datum-confusion class returns.** A
    /// caller holding two [`ReferenceElevation`]s should subtract them instead —
    /// that path cannot be wrong about which datum it is on. Finiteness is a
    /// `debug_assert!` only, matching [`TempAnomaly::from_offset_c`].
    /// type-audit: bare-ok(constructor-edge: value)
    pub fn from_metres(value: f64) -> Self {
        debug_assert!(value.is_finite(), "sea-level height must be finite");
        Self(value)
    }

    /// The raw signed metres above sea level.
    /// type-audit: bare-ok(constructor-edge: return)
    pub fn get(self) -> f64 {
        self.0
    }

    /// Metres *below* sea level — the positive-downward reading, and the
    /// negation of [`get`](Self::get). This accessor exists so that a consumer
    /// wanting depth never writes the negation by hand: a stray sign is the
    /// same confusion class this type exists to remove.
    /// type-audit: bare-ok(constructor-edge: return)
    pub fn depth(self) -> f64 {
        -self.0
    }

    /// Deterministic total order via `f64::total_cmp` (no NaN ambiguity).
    pub fn total_cmp(self, other: Self) -> Ordering {
        self.0.total_cmp(&other.0)
    }
}
```

- [ ] **Step 4: Export it**

In `kernel/src/lib.rs`, add `SeaLevelHeight` to the same `pub use` list that
already exports `ReferenceElevation`. Find it with:
`grep -n "ReferenceElevation" kernel/src/lib.rs`

- [ ] **Step 5: Run the tests to verify they pass**

Run: `cargo test -p hornvale-kernel --lib units`
Expected: PASS, 3 new tests.

- [ ] **Step 6: Gate and commit**

```bash
cargo fmt
cargo clippy -p hornvale-kernel --all-targets -- -D warnings
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
git add kernel/src/units.rs kernel/src/lib.rs
git commit -F <message-file>
```

Commit message subject: `feat(kernel): SeaLevelHeight, the datum ReferenceElevation is not`
Write the message to a file and use `git commit -F` — never a heredoc (`PROC-commit-message-via-file`).

---

### Task 2: The named conversion  ✅ DONE (revised in flight)

**Status: complete, committed with Task 1's follow-up.** This task originally
retyped `Sub for ReferenceElevation`. The implementer ran the compiler, got 21
errors, and correctly stopped rather than silencing them. Three named sites that
are **not sea-level quantities at all** —
`domains/climate/src/moisture.rs:169`, `domains/climate/src/provider.rs:182`
(orographic rise between a cell and its upwind neighbour) — proved that
subtracting two elevations is polymorphic in meaning, so the operator cannot
carry a datum-named output type.

Replaced by a **named conversion**, per decision 0008's "validating constructors
and *named conversions*":

```rust
pub fn above(self, datum: Self) -> SeaLevelHeight {
    SeaLevelHeight(self.0 - datum.0)
}
```

`Sub` keeps `type Output = f64`, documented with *why* it is deliberately not a
`SeaLevelHeight`. Workspace blast radius: **zero** — `cargo check --workspace
--all-targets` is clean, against 21 errors for the operator approach.

Enforcement is unaffected: it lives in the *parameter type* of consuming
functions (Task 4's `relief_band`), never in the operator. See spec §4.1's
post-G3 amendment and §4.4.

**For the remaining tasks:** wherever the plan below says to subtract an
elevation from a sea level to get a height, call `.above(sea_level)` instead.
The two spellings that follow are equivalent, and the second is the one to use:

```rust
let h = SeaLevelHeight::from_metres(elevation.get() - sea_level.get()); // no
let h = elevation.above(sea_level);                                     // yes
```

`from_metres` remains correct in exactly one place — Task 3's blended value,
where there is no pair of `ReferenceElevation`s to convert.

---


### Task 3: `LocaleFields` carries the height

**Files:**
- Modify: `windows/locale/src/lib.rs:96-118` (`LocaleFields`), `:505-515` (the blend), and the serializer region near `serialize_water_kind`
- Test: `windows/locale/src/lib.rs` test module

**Interfaces:**
- Consumes: `SeaLevelHeight`
- Produces: `LocaleFields.height_asl_m: SeaLevelHeight`, serialized as a quantized `f64` under the JSON key `height_asl_m`

**Schema note:** this is an **additive** change to `locale/room/v2`, so no
version bump (decision 0055's additive-or-versioned rule). It changes the
committed artifact `book/src/reference/locale-seed-42.json`, regenerated in
Task 4's regen step.

- [ ] **Step 1: Write the failing test**

```rust
#[test]
fn a_locale_reports_height_above_sea_level_not_the_raw_reading() {
    let world = land_world();
    let ctx = LocaleContext::build(&world).unwrap();
    // The same address `fields_are_within_the_corner_range` uses, for the same
    // reason: it resolves on seed 42's mesh without needing a settlement.
    let addr = RoomAddr {
        face: 3,
        path: vec![0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3],
    };
    let loc = ctx.describe(&addr, WorldTime { day: 0.0 }).unwrap();
    let sea = hornvale_kernel::quantize(ctx.terrain().globe().sea_level.get());
    let expected = hornvale_kernel::quantize(loc.fields.elevation_m - sea);
    assert_eq!(
        loc.fields.height_asl_m.get(),
        expected,
        "height_asl_m must be elevation_m re-datumed onto sea level, exactly"
    );
}
```

`land_world()` and that `RoomAddr` literal are the module's existing fixtures
(`windows/locale/src/lib.rs:1001` and `:1072`) — do not add new ones. The
assertion is an exact `assert_eq!` rather than an epsilon because both sides are
quantized at the same boundary; if it is ever not exact, Step 5 got the
quantization order wrong and the self-consistency test in Task 4 will fail too.

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test -p hornvale-locale --lib`
Expected: FAIL to compile — `no field height_asl_m on type LocaleFields`.

- [ ] **Step 3: Add the field**

In `LocaleFields`, after `elevation_m`:

```rust
    /// Height above this world's sea level, metres — signed, negative below.
    /// `elevation_m` is the absolute isostatic reading and stays beside it,
    /// because every correct consumer already reads that one; this is the
    /// quantity a *reader* wants, and the one the relief bands are computed
    /// from (The Benchmark).
    #[serde(serialize_with = "serialize_height_asl")]
    pub height_asl_m: SeaLevelHeight,
```

**Do NOT add a `type-audit:` tag for this field.** ✅ *Corrected in flight —
the original instruction said to add `bare-ok(constructor-edge: height_asl_m)`,
and the tool rejects it:* `locale:104: stale tag position height_asl_m`. The
audit tracks **primitives** at pub edges; `height_asl_m` is a `SeaLevelHeight`,
a newtype, so it is not an audited position and a tag for it is stale by
definition (`tools/type-audit/CLAUDE.md`). Leave the struct's tag line as it is.

The distinction matters for Task 4, which adds `sea_level_m: f64` and
`height_asl_m: Option<f64>` — those **are** bare primitives at a pub boundary
and **do** need tags. Typing a field is what removes the tag obligation; naming
it does not.

- [ ] **Step 4: Add the serializer**

Beside `serialize_water_kind`:

```rust
/// Serialize a [`SeaLevelHeight`] as its quantized metres — the emit-boundary
/// quantization every float in this schema goes through (decision 0033). The
/// type cannot travel through JSON, so the *field name* carries the datum
/// instead; that pairing is the whole discipline.
fn serialize_height_asl<S: serde::Serializer>(
    h: &SeaLevelHeight,
    s: S,
) -> Result<S::Ok, S::Error> {
    s.serialize_f64(quantize(h.get()))
}
```

- [ ] **Step 5: Populate it in the blend**

In `describe_with_weights`, inside the `LocaleFields` literal (around :510):

The order of operations matters and is load-bearing. `elevation_m` is *already*
quantized (`blend` applies `quantize` to its result), so the height must be
derived from that quantized value and a quantized sea level — otherwise the
emitted `height_asl_m` and the band computed from it can straddle a boundary
differently from what a client re-derives from the document.

Build the fields in two steps so the dependency is explicit:

```rust
        let elevation_m = blend(&|c| self.terrain.globe().elevation.get(c).get());
        // `from_metres`, not a subtraction: the left operand is a three-corner
        // BLEND, not any single cell's reading, so there is no pair of
        // `ReferenceElevation`s here to subtract. Derived from the already-
        // quantized `elevation_m` and a quantized sea level so that the value
        // emitted and the band computed from it agree exactly with what a
        // consumer re-derives from the document.
        let sea_level_m = quantize(self.terrain.globe().sea_level.get());
        let fields = LocaleFields {
            temperature_c: blend(&|c| self.climate.mean_temperature_at(c).get()),
            moisture: blend(&|c| self.climate.moisture_at(c)),
            elevation_m,
            height_asl_m: SeaLevelHeight::from_metres(quantize(elevation_m - sea_level_m)),
            water: *self.terrain.globe().water_kind.get(best.0),
        };
```

- [ ] **Step 6: Run to verify it passes**

Run: `cargo test -p hornvale-locale --lib`
Expected: PASS.

- [ ] **Step 7: Gate and commit**

```bash
cargo fmt
cargo clippy -p hornvale-locale --all-targets -- -D warnings
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
git add windows/locale/src/lib.rs && git commit -F <message-file>
```
Subject: `feat(locale): a room reports its height above sea level`

---

### Task 4: `scene/surrounds/v2` — the correctness fix

The band correction and the schema bump land together. Changing band values
without bumping the schema would violate v1's published promise even
transiently, and the golden re-pins in this same commit (never deferred to the
close).

**Files:**
- Modify: `windows/scene/src/surrounds.rs` — `:15` schema const, `:30-40` `relief_band`, `:110-135` `SurroundsCell`, `:136-167` `SurroundsScene`, `:251-255` the builder
- Rename: `book/src/reference/scene-surrounds-v1.md` → `book/src/reference/scene-surrounds-v2.md`
- Modify: `book/src/SUMMARY.md` (the reference entry)
- Regenerate: `book/src/gallery/scene-surrounds-seed-42.json`, `book/src/gallery/generated/surrounds-seed-42/*.txt`, `book/src/reference/locale-seed-42.json`
- Test: `windows/scene/src/surrounds.rs` test module

**Interfaces:**
- Consumes: `LocaleFields.height_asl_m` (Task 3), `SeaLevelHeight`
- Produces: `SURROUNDS_SCHEMA == "scene/surrounds/v2"`; `relief_band(h: SeaLevelHeight) -> u32`; `SurroundsScene.sea_level_m: f64`; `SurroundsCell.height_asl_m: Option<f64>`

- [ ] **Step 1: Write the failing tests**

The first is the one that matters. A unit test on `relief_band` alone would pass
both before *and* after the fix, because the defect is at the **call site** — so
the test asserts the emitted document is self-consistent.

```rust
#[test]
fn the_emitted_relief_band_matches_the_emitted_height() {
    // THE test for this defect. A unit test on `relief_band` alone would pass
    // both before and after the fix, because what was wrong is which argument
    // the CALL SITE passes. This pins the band to the height in the same
    // document, so passing the raw reading again breaks it.
    let w = world();
    let ctx = hornvale_locale::LocaleContext::build(&w).unwrap();
    let scene = surrounds_scene_in(&w, &ctx, &observer(&w), 2, WorldTime { day: 0.0 }).unwrap();
    let mut checked = 0;
    for c in &scene.cells {
        if let Some(h) = c.height_asl_m {
            assert_eq!(
                c.relief,
                relief_band(SeaLevelHeight::from_metres(h)),
                "room {} bands as {} but sits {h} m above sea level",
                c.room,
                scene.relief_legend[c.relief as usize],
            );
            checked += 1;
        }
    }
    assert!(checked > 0, "at least the observer's own cell carries a height");
}

#[test]
fn the_document_carries_the_datum_its_bands_are_measured_from() {
    let w = world();
    let ctx = hornvale_locale::LocaleContext::build(&w).unwrap();
    let scene = surrounds_scene_in(&w, &ctx, &observer(&w), 1, WorldTime { day: 0.0 }).unwrap();
    assert_eq!(scene.schema, "scene/surrounds/v2");
    assert_eq!(
        scene.sea_level_m,
        hornvale_kernel::quantize(ctx.terrain().globe().sea_level.get()),
        "a client cannot re-derive a band without the datum"
    );
}

#[test]
fn no_land_cell_bands_as_marine_relief() {
    // Seed 42's sea level is -2936.17 m, so banding the RAW isostatic reading
    // put 8162 of this world's 11,066 land cells in `shelf` and left the planet
    // exactly one `alpine` cell. Stated over CELLS, where the invariant holds by
    // definition: a land cell IS one with `elevation >= sea_level`, so its
    // height is >= 0 and its band must be `lowland` or above.
    //
    // Deliberately NOT stated over rooms: a room's height is a three-corner
    // blend while its water kind is a point sample of the dominant corner, so a
    // shoreline room can be dry-land-dominant and still blend centimetres below
    // sea level. That asymmetry is real and out of scope (spec §12.4).
    let w = world();
    let ctx = hornvale_locale::LocaleContext::build(&w).unwrap();
    let globe = ctx.terrain().globe();
    let sea = globe.sea_level;
    let mut land = 0usize;
    for (cell, e) in globe.elevation.iter() {
        if e.total_cmp(sea) == std::cmp::Ordering::Less {
            continue;
        }
        land += 1;
        let band = relief_band(*e - sea);
        assert!(
            band >= 2,
            "land cell {cell:?} at {:.1} m ({:.1} m above sea level) banded as {}",
            e.get(),
            (*e - sea).get(),
            RELIEF_LEGEND[band as usize]
        );
    }
    assert!(land > 1000, "seed 42 has substantial land; got {land} cells");
}
```

- [ ] **Step 2: Run to verify they fail**

Run: `cargo test -p hornvale-scene --lib surrounds`
Expected: FAIL to compile (`no field height_asl_m`, `no field sea_level_m`).

**Then earn a real RED.** `no_land_cell_bands_as_marine_relief` depends on no new
struct field — only on `relief_band` taking a `SeaLevelHeight`. So after Step 3
(retyping the function) and *before* Step 5 (fixing the call site), run:

`cargo test -p hornvale-scene --lib no_land_cell_bands_as_marine_relief`

It must fail on its **assertion**, naming a land cell banded `shelf` — not fail
to compile. Record that message in the commit body. A test that has only ever
been seen to fail-to-compile has not been shown to detect anything (The
Timekeeper: 8 of 16 defects were inside the detector, and only "require RED"
caught them).

- [ ] **Step 3: Retype the band function**

```rust
/// Height above sea level to an index into [`RELIEF_LEGEND`].
///
/// The parameter is a [`SeaLevelHeight`] and not a `ReferenceElevation` for the
/// reason The Benchmark exists: these thresholds are sea-level-relative, and
/// before v2 this function was handed the raw isostatic reading, so on a world
/// whose sea level sits near -2936 m almost all land classified as `shelf`.
/// type-audit: bare-ok(index: return)
fn relief_band(height: SeaLevelHeight) -> u32 {
    match height.get() {
        e if e < -3000.0 => 0,
        e if e < 0.0 => 1,
        e if e < 300.0 => 2,
        e if e < 1000.0 => 3,
        e if e < 2500.0 => 4,
        _ => 5,
    }
}
```

- [ ] **Step 4: Bump the schema and add the fields**

`:15` → `pub const SURROUNDS_SCHEMA: &str = "scene/surrounds/v2";`

In `SurroundsScene`, after `relief_legend` (field order is JSON key order and is
contract — append, never reorder):

```rust
    /// This world's derived sea level, metres on the isostatic datum. The
    /// bands in `relief_legend` are measured from it, so a consumer can
    /// re-derive any cell's band from `height_asl_m` alone. Its absence from
    /// v1 left the one scene kind whose bands were wrong also the one kind a
    /// client could not correct.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub sea_level_m: f64,
```

Add `bare-ok(diagnostic-value: sea_level_m)` to the struct's `type-audit:` tag.

In `SurroundsCell`, after `elevation_m`:

```rust
    /// Height above sea level, metres — fine grain, `null` when coarse.
    /// Signed: negative below. `relief` is banded from this.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::opt_f64_field")]
    pub height_asl_m: Option<f64>,
```

Add `bare-ok(diagnostic-value: height_asl_m)` to that struct's tag.

- [ ] **Step 5: Fix the call site and populate**

At `:251`:
```rust
            relief: relief_band(locale.fields.height_asl_m),
```
and after the `elevation_m` line:
```rust
            height_asl_m: is_here.then_some(locale.fields.height_asl_m.get()),
```

In the `SurroundsScene` literal near `:273`, beside `schema:`:
```rust
        sea_level_m: ctx.terrain().globe().sea_level.get(),
```

Fix the other constructors the compiler names —
`windows/scene/src/surrounds_ascii.rs:240` builds a `SurroundsCell` literal and
needs `height_asl_m: None`.

- [ ] **Step 6: Add the multi-seed sweep to the heavy tier**

One world is an anecdote, and seed 42's sea level being far from zero is exactly
the kind of accident another seed might not share — a seed whose sea level lands
near 0 m would make the raw and corrected bands agree and hide the bug.

```rust
#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn no_land_cell_bands_as_marine_relief_across_seeds() {
    for seed in [1u64, 7, 42, 99, 2026] {
        let w = build_world(
            Seed(seed),
            &hornvale_astronomy::SkyPins::default(),
            SkyChoice::Generated,
            &hornvale_terrain::TerrainPins::default(),
            &SettlementPins::default(),
        )
        .expect("the seed builds");
        let ctx = hornvale_locale::LocaleContext::build(&w).unwrap();
        let globe = ctx.terrain().globe();
        let sea = globe.sea_level;
        for (cell, e) in globe.elevation.iter() {
            if e.total_cmp(sea) == std::cmp::Ordering::Less {
                continue;
            }
            let band = relief_band(*e - sea);
            assert!(
                band >= 2,
                "seed {seed}: land cell {cell:?} banded as {}",
                RELIEF_LEGEND[band as usize]
            );
        }
        // The datum's distance from zero is what made this defect invisible;
        // print it so a future reader can see the spread across seeds.
        println!("seed {seed}: sea level {:.1} m", sea.get());
    }
}
```

The `#[ignore]` reason string is checked **verbatim** by `cli/tests/heavy_tier.rs`
— not as a prefix. A bespoke reason reddens the gate. Copy it exactly.

- [ ] **Step 7: Run to verify they pass**

```bash
cargo nextest run -p hornvale-scene 2>&1 | tee /tmp/hv-t4.txt
cargo nextest run -p hornvale-scene --run-ignored all -E 'test(across_seeds)' 2>&1 | tee /tmp/hv-t4-heavy.txt
cargo test -p hornvale --test heavy_tier
```
Expected: the three commit-gate tests PASS; the heavy sweep PASSES and prints
five sea levels; `heavy_tier` accepts the new ignore reason.

- [ ] **Step 8: Rewrite the reference page as v2**

```bash
git mv book/src/reference/scene-surrounds-v1.md book/src/reference/scene-surrounds-v2.md
```

Then edit it: every `scene/surrounds/v1` → `v2`; the band table's column header
becomes **Height above sea level (m)** with the same boundaries; and replace the
sentence at the old `:180` with:

> Each band is half-open, `[lower, upper)`, against `height_asl_m` — **not**
> against `elevation_m`, which is an absolute reading on the planet-independent
> isostatic datum whose zero is nowhere near any particular world's sea level.
> Banding the absolute reading is what v1 did, and on a world whose sea level
> sits near −2936 m it classified almost all land as `shelf`. Changing a
> boundary, or the quantity they are measured against, mints
> `scene/surrounds/v3`.

Document `sea_level_m` and `height_asl_m` in the field tables (the two tables at
roughly `:220` and `:247`). Update `book/src/SUMMARY.md`'s entry, and grep for
stale inbound links:
`grep -rn "scene-surrounds-v1" book/ docs/ clients/`

- [ ] **Step 9: Regenerate and review the artifact diff**

```bash
make rebaseline
git diff --stat book/src/gallery/ book/src/reference/ docs/audits/
python3 -c "
import json,collections
d=json.load(open('book/src/gallery/scene-surrounds-seed-42.json'))
print('schema:', d['schema'], '| has sea_level_m:', 'sea_level_m' in d)
print('relief histogram:', dict(sorted(collections.Counter(c['relief'] for c in d['cells']).items())))
"
```
Expected: schema `scene/surrounds/v2`, `sea_level_m` present, and a relief
histogram **other than** `{1: 31}`. Confirm the diff is confined to the
artifacts this plan's File Structure names; anything else is a finding.

- [ ] **Step 10: Gate and commit**

```bash
cargo fmt
cargo clippy --workspace --all-targets -- -D warnings
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
cargo test -p hornvale --test docs_consistency
git add -A && git commit -F <message-file>
```
Subject: `fix(scene): relief bands measure height above sea level (v2)`

The commit body must quote the RED assertion message observed in Step 2 and the
before/after relief histogram from Step 9 — the golden re-pins in this same
commit, so the evidence that it *should* move belongs here (re-pin a witness,
never a claim).

---

### Task 5: What a walker is told

**Files:**
- Modify: `windows/vessel/src/focalize.rs:72-81`
- Modify: `cli/src/main.rs` (the `locale` renderer's `temperature … elevation` line — find with `grep -n "elevation" cli/src/main.rs`)
- Test: `windows/vessel/src/focalize.rs` test module

**Interfaces:**
- Consumes: `LocaleFields.height_asl_m`
- Produces: no new API; prose only

- [ ] **Step 1: Write the failing test**

```rust
#[test]
fn the_biome_datum_reports_height_above_sea_level() {
    let v = vantage_at(0.0);
    let f = TemplateFocalizer.render(&v);
    let (_, datum) = f
        .nouns
        .iter()
        .find(|(n, _)| *n == v.locale.biome)
        .expect("the biome is a noun");
    // Seed 42's sea level is -2936.17 m. Before The Benchmark this line read
    // "-2936 m elevation" for a tropical forest at the shoreline.
    assert!(
        !datum.contains("-2936"),
        "the raw isostatic reading leaked into prose: {datum}"
    );
    assert!(
        datum.contains("above sea level") || datum.contains("below sea level"),
        "a height must say what it is a height above: {datum}"
    );
}
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test -p hornvale-vessel --lib focalize`
Expected: FAIL — the datum contains `-2936`.

- [ ] **Step 3: Fix the prose**

In `focalize.rs`, replace the biome noun's datum expression (`:75-81`) with:

```rust
                format!(
                    "{:.1} °C the year round, moisture {:.2}, {}.",
                    v.locale.fields.temperature_c,
                    v.locale.fields.moisture,
                    height_phrase(v.locale.fields.height_asl_m)
                ),
```

and add above `impl Focalizer`:

```rust
/// A height as a reader-facing phrase. Sea level is derived per world and is
/// nowhere near 0 m on the isostatic datum, so a bare signed number is not
/// merely unhelpful — it reads as a depth. Saying the datum aloud is the prose
/// half of the discipline the type carries in code.
/// type-audit: bare-ok(prose: return)
fn height_phrase(h: SeaLevelHeight) -> String {
    let m = h.get();
    if m < 0.0 {
        format!("{:.0} m below sea level", h.depth())
    } else {
        format!("{m:.0} m above sea level")
    }
}
```

- [ ] **Step 4: Fix the CLI renderer the same way**

`cli/src/main.rs:1603-1606` currently reads:

```rust
        println!(
            "  temperature {:.1} °C · moisture {:.2} · elevation {:.0} m",
            locale.fields.temperature_c, locale.fields.moisture, locale.fields.elevation_m
        );
```

Replace with:

```rust
        let h = locale.fields.height_asl_m;
        let height = if h.get() < 0.0 {
            format!("{:.0} m below sea level", h.depth())
        } else {
            format!("{:.0} m above sea level", h.get())
        };
        println!(
            "  temperature {:.1} °C · moisture {:.2} · {height}",
            locale.fields.temperature_c, locale.fields.moisture
        );
```

The two-branch format is repeated rather than shared: `cli` must not reach into
`vessel`'s private helpers, and four lines do not justify a new pub API on either
crate. If a third surface ever needs it, promote it to `windows/locale` then —
not now (YAGNI).

Note this changes `book/src/gallery/locale-*.md` and the `--sample`/`--strange`
pages, all regenerated in Step 6.

- [ ] **Step 5: Run to verify green**

```bash
cargo nextest run -p hornvale-vessel -p hornvale 2>&1 | tee /tmp/hv-t5.txt
cargo run -p hornvale -- possess --seed 42 --script /tmp/hv-look.txt
```
with `/tmp/hv-look.txt` containing `examine tropical seasonal forest` then
`quit`. Expected: a height phrase naming sea level, and no `-2936`.

- [ ] **Step 6: Regenerate, gate, commit**

```bash
make rebaseline
git diff --stat book/
cargo fmt && cargo clippy --workspace --all-targets -- -D warnings
git add -A && git commit -F <message-file>
```
Subject: `fix(vessel): a walker is told a height, not an isostatic reading`

---

### Task 6: Retire the misleading names and the waiver

**Files:**
- Modify: `windows/worldgen/src/lib.rs:1653` and the struct whose `elevation` field it fills (find with `grep -n "pub elevation" windows/worldgen/src/lib.rs domains/species/src/lib.rs`)
- Modify: `docs/design/kernel-units-doctrine.md` (the `elevation-convention` waiver's status)
- Modify: `windows/locale/src/lib.rs:100` — drop `waiver(elevation-convention: elevation_m)` only if the audit agrees
- Test: existing suites; no new behaviour

- [ ] **Step 1: Rename the field that holds a difference**

`windows/worldgen/src/lib.rs:1653` computes a height and stores it in a field
named `elevation` — correct arithmetic under a name that says the opposite.
Rename the field to `height_asl_m`, give it type `SeaLevelHeight`, drop the
`.get()` added in Task 2 Step 5, and update every reader the compiler names.

- [ ] **Step 2: Run the affected suites**

Run: `cargo nextest run -p hornvale-worldgen -p hornvale-species 2>&1 | tee /tmp/hv-t6.txt`
Expected: PASS, no value changes — this is a rename plus a type, not a
computation change.

- [ ] **Step 3: Settle the waiver's status honestly**

Run: `cargo run --manifest-path tools/type-audit/Cargo.toml -- check`

`docs/design/kernel-units-doctrine.md:23` calls the `elevation-convention`
waiver *temporary*, retired by the kernel type. Determine which sites still
carry it and why:
`grep -rn "elevation-convention" --include=*.rs .`

Retire the tag where the new type genuinely replaces a bare `f64`. Where a site
keeps it — `LocaleFields.elevation_m` legitimately remains an absolute reading —
say so in the doctrine rather than leaving the waiver looking unexamined. Do not
claim a full retirement the code does not support.

- [ ] **Step 4: Regenerate the audit report**

```bash
cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md
git diff --stat docs/audits/
```

- [ ] **Step 5: Full gate**

```bash
cargo fmt
make gate 2>&1 | tee /tmp/hv-gate.txt
```
Budget `timeout: 3600000` — `make gate` has measured 22–37 min in a worktree.
Read the file for the failure list; trust the exit code.

- [ ] **Step 6: Commit**

```bash
git add -A && git commit -F <message-file>
```
Subject: `refactor(worldgen): name the datum, and settle the elevation waiver`

---

## Close (not a task — the campaign close skill owns it)

`closing-a-campaign` covers the chronicle entry, the Confidence Gradient
re-score if any bet moved, the retrospective, promotion of
`.superpowers/sdd/followups.md`, and the G6 ledger digest. The three registry
rows are already filed (commit `9767ecea`); flip none of them — they are
follow-ups this campaign deliberately does not do.

Book freshness sweep must include: `book/src/chronicle/the-purview.md` and
`the-margin.md` (both describe `scene/surrounds/v1` by name),
`book/src/reference/scene-surrounds-v2.md`, and any chapter quoting an elevation
figure for seed 42.
