# Cross-Platform Float Quantization Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make every committed, byte-checked artifact byte-identical across platforms (macOS/arm64 dev ↔ Linux/x86_64 CI) by quantizing serialized floats at each artifact boundary — turning green a CI that has been red on every push since 2026-07-07.

**Architecture:** Rust's `f64` transcendentals (`sin`/`cos`/`atan2`/`powf`) dispatch to the platform libm, which is not bit-identical between Apple's libm and glibc. Those last-ULP differences reach the committed golden files and fail the byte-exact drift checks. The fix is one kernel primitive, `quantize(f64) -> f64`, that rounds to a fixed number of significant decimal digits using Rust's **libm-free, platform-independent** float formatting/parsing (Grisu/Dragon + Eisel-Lemire, both pure-Rust in libcore). It is applied at each of the four serialization boundaries that carry high-precision floats. This is a deliberate **save-format epoch**: every committed world, almanac-adjacent artifact, scene dump, ephemeris, and lab census is regenerated in the same change.

**Tech Stack:** Rust (edition 2024, toolchain pinned 1.96.1), `serde`/`serde_json` only. No new crates.

## Global Constraints

- Dependencies: `serde` + `serde_json` only, workspace-wide. **No new crates** (no `libm` crate — the whole point is to stay libm-free using std formatting).
- No `HashMap`/`HashSet`; no wall-clock time. (Not touched here, but the gate enforces it.)
- Every crate `#![warn(missing_docs)]`; every public item/field/variant gets a one-line doc comment.
- Every primitive at a `pub` boundary carries a `type-audit:` verdict tag; `tools/type-audit` `check` is default-deny and drift-checked.
- Determinism is constitutional: same seed + pins → byte-identical output. Debug and release builds must stay byte-identical (`release_determinism` test).
- `cargo fmt` is the final step before every commit. Full gate: `cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings`.
- Deliberate regeneration uses an epoch suffix, never a rename — but this change alters **stored fact values**, not stream labels, so no `stream_labels` change is needed; the epoch is recorded in the decision log and chronicle.

## The four drift boundaries (measured on HEAD, high-precision floats per artifact)

| Boundary | Artifact(s) | High-precision floats | Choke point |
|---|---|---|---|
| Ledger | `cli/tests/fixtures/world-seed-42.json` | 127 | `Ledger::commit` (kernel) |
| Lab CSV | `book/src/laboratory/generated/*/rows.csv` | 10,799 + 20,037 | `render_csv` (windows/lab) |
| Scene JSON | `book/src/gallery/scene-tiles-seed-42.json`, `scene-system-seed-42.json` | 61,625 + 16 | scene artifact structs (windows/scene) |
| Ephemeris | `clients/orrery/testdata/ephemeris-seed-42.json` | 75 | scene ephemeris struct (windows/scene) |

Already safe (0 high-precision floats — coarse `{:.0}`/`{:.1}` or pure text): all three almanacs, `elevation-seed-42.md`, `dictionary-generated.md`, concept/stream/phonology reference dumps. **Do not touch their producers.** The PNG maps quantize to pixels; they are verified in Task 6, not pre-emptively changed.

---

### Task 1: Kernel `quantize` primitive

**Files:**
- Modify: `kernel/src/lib.rs` (add `mod quantize;` + re-export)
- Create: `kernel/src/quantize.rs`

**Interfaces:**
- Produces: `hornvale_kernel::quantize(x: f64) -> f64`; `hornvale_kernel::QUANTIZE_SIG_DIGITS: u32`; serde helpers `hornvale_kernel::quantize_serde::{f64_field, opt_f64_field, vec_f64_field}` (module `quantize_serde`).

- [ ] **Step 1: Write the failing tests** in `kernel/src/quantize.rs`

```rust
//! Deterministic, platform-independent float quantization. Rust's `f64`
//! transcendentals route to the platform libm (Apple's vs glibc's), which
//! differ in the last ULP; those differences reach committed golden files
//! and break byte-exact drift checks across platforms. `quantize` rounds a
//! float to a fixed number of significant decimal digits using Rust's
//! libm-free float formatting/parsing (identical on every platform), so two
//! values differing only by sub-quantum libm noise collapse to the same
//! `f64`. This is a save-format contract: changing `QUANTIZE_SIG_DIGITS`
//! silently re-bases every committed artifact.

use serde::{Serialize, Serializer};

/// Significant decimal digits retained by [`quantize`]. Eight leaves ~7–8
/// digits of margin over 15–16-digit ULP noise (widening the safety margin
/// against future boundary-flip re-pins) while preserving far more physical
/// precision than a text sim observes — and quantization touches only
/// serialized outputs, never the compute path, so the digit count has zero
/// effect on simulation fidelity. Save-format constant.
pub const QUANTIZE_SIG_DIGITS: u32 = 8;

/// Round `x` to [`QUANTIZE_SIG_DIGITS`] significant digits, deterministically
/// and identically on every platform. Non-finite inputs pass through
/// unchanged (the ledger rejects them elsewhere).
/// type-audit: bare-ok(artifact)
pub fn quantize(x: f64) -> f64 {
    if !x.is_finite() {
        return x;
    }
    // `{:.*e}` uses core::fmt's Grisu/Dragon formatter (pure Rust, no libm);
    // parse uses core::num::dec2flt (Eisel-Lemire, also pure Rust). Both are
    // bit-for-bit identical across platforms for identical input bits.
    let precision = (QUANTIZE_SIG_DIGITS - 1) as usize;
    let s = format!("{x:.precision$e}");
    s.parse::<f64>().expect("quantize: re-parse of formatted finite f64")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn quantize_is_idempotent() {
        let v = 368.05357093462703_f64;
        assert_eq!(quantize(v), quantize(quantize(v)));
    }

    #[test]
    fn one_ulp_neighbors_collapse_to_the_same_value() {
        // The cross-platform property in miniature: two values differing by a
        // single ULP (as libm implementations do) quantize equal.
        let v = 210.2242156495795_f64;
        let up = f64::from_bits(v.to_bits() + 1);
        assert_ne!(v, up, "precondition: distinct bit patterns");
        assert_eq!(quantize(v), quantize(up));
    }

    #[test]
    fn retains_ten_significant_digits_across_magnitudes() {
        assert_eq!(quantize(0.041161436763_f64), 0.04116143676);
        assert_eq!(quantize(4328.497184910964_f64), 4328.497185);
    }

    #[test]
    fn non_finite_and_zero_pass_through() {
        assert_eq!(quantize(0.0), 0.0);
        assert!(quantize(f64::NAN).is_nan());
        assert_eq!(quantize(f64::INFINITY), f64::INFINITY);
    }
}
```

- [ ] **Step 2: Run to verify it fails** — `cargo test -p hornvale-kernel quantize` — Expected: FAIL (module not wired up / not found).

- [ ] **Step 3: Wire the module** in `kernel/src/lib.rs`: add `mod quantize;` and `pub use quantize::{QUANTIZE_SIG_DIGITS, quantize};` alongside the existing re-exports (match the file's existing `pub use` style).

- [ ] **Step 4: Add the serde helpers** to `kernel/src/quantize.rs` (used by Task 4). Keep them in a `pub mod quantize_serde`:

```rust
/// serde `serialize_with` adapters that quantize on the way out, so artifact
/// structs stay full-precision in memory but serialize canonically.
pub mod quantize_serde {
    use super::quantize;
    use serde::{Serialize, Serializer};

    /// Quantize an `f64` field. type-audit: bare-ok(artifact)
    pub fn f64_field<S: Serializer>(x: &f64, s: S) -> Result<S::Ok, S::Error> {
        quantize(*x).serialize(s)
    }

    /// Quantize an `Option<f64>` field. type-audit: bare-ok(artifact)
    pub fn opt_f64_field<S: Serializer>(x: &Option<f64>, s: S) -> Result<S::Ok, S::Error> {
        x.map(quantize).serialize(s)
    }

    /// Quantize a `Vec<f64>` field. type-audit: bare-ok(artifact)
    pub fn vec_f64_field<S: Serializer>(x: &[f64], s: S) -> Result<S::Ok, S::Error> {
        x.iter().copied().map(quantize).collect::<Vec<_>>().serialize(s)
    }
}
```
Remove the unused top-of-file `use serde::{Serialize, Serializer};` if it lints; the helpers import locally.

- [ ] **Step 5: Run tests + clippy** — `cargo test -p hornvale-kernel quantize && cargo clippy -p hornvale-kernel --all-targets -- -D warnings` — Expected: PASS, no warnings.

- [ ] **Step 6: Commit** — `git add kernel/ && git commit -m "feat(kernel): quantize — libm-free float canonicalization for cross-platform artifacts"`

---

### Task 2: Quantize at the ledger boundary (fixes `world.json`)

**Files:**
- Modify: `kernel/src/ledger.rs` (`Ledger::commit`, plus the `Value` doc comment)
- Regenerate: `cli/tests/fixtures/world-seed-42.json`

**Interfaces:**
- Consumes: `hornvale_kernel::quantize`.

- [ ] **Step 1: Write the failing test** in `kernel/src/ledger.rs` `tests`:

```rust
#[test]
fn committed_numbers_and_days_are_quantized() {
    use crate::quantize::quantize;
    let r = registry();
    let mut l = Ledger::default();
    let e = l.mint_entity();
    let raw = 210.2242156495795_f64;
    l.commit(
        Fact {
            subject: e,
            predicate: "name".to_string(),
            object: Value::Number(raw),
            place: None,
            day: Some(raw),
            provenance: "test".to_string(),
        },
        &r,
    )
    .unwrap();
    let stored = l.iter().next().unwrap();
    assert_eq!(stored.object, Value::Number(quantize(raw)));
    assert_eq!(stored.day, Some(quantize(raw)));
}
```

- [ ] **Step 2: Run to verify it fails** — `cargo test -p hornvale-kernel committed_numbers_and_days_are_quantized` — Expected: FAIL (stored value is raw, not quantized).

- [ ] **Step 3: Quantize in `commit`.** Change the signature to take `mut fact` and canonicalize before `check`/dedup so idempotency and contradiction checks compare canonical values:

```rust
pub fn commit(&mut self, mut fact: Fact, registry: &ConceptRegistry) -> Result<bool, LedgerError> {
    if let Value::Number(n) = fact.object {
        fact.object = Value::Number(crate::quantize::quantize(n));
    }
    fact.day = fact.day.map(crate::quantize::quantize);
    self.check(&fact, registry)?;
    if self.facts.contains(&fact) {
        return Ok(false);
    }
    self.facts.push(fact);
    Ok(true)
}
```
Update the `Value` doc comment (lines ~13–15): the "bitwise-exact f64 equality" note should now read that number objects are quantized at commit to a platform-stable canonical form (cite the quantize module).

- [ ] **Step 4: Run tests** — `cargo test -p hornvale-kernel` — Expected: PASS (existing ledger tests still hold; `finite_numbers_still_commit` uses 42.5/3.0 which quantize to themselves).

- [ ] **Step 5: Regenerate the world fixture and confirm the guard passes.**

```bash
cargo run -p hornvale -- new --seed 42 --out /tmp/hv-42.json
cp /tmp/hv-42.json cli/tests/fixtures/world-seed-42.json
cargo test -p hornvale --test lens_purity
```
Expected: `seed_42_world_json_matches_the_committed_fixture ... ok`.

- [ ] **Step 6: Commit** — `git add kernel/ cli/tests/fixtures/world-seed-42.json && git commit -m "feat(kernel): quantize ledger numbers at commit; re-freeze seed-42 world (save-format epoch)"`

---

### Task 3: Quantize the lab CSV (fixes both censuses)

**Files:**
- Modify: `windows/lab/src/runner.rs` (`render_csv`, the `MetricValue::Number` arm)
- Regenerate: `book/src/laboratory/generated/census-lands-drift/rows.csv`, `book/src/laboratory/generated/census-of-the-meeting/rows.csv`, and any per-study summary/chart the runner rewrites.

**Interfaces:**
- Consumes: `hornvale_kernel::quantize`.

- [ ] **Step 1: Write the failing test** near `render_csv` in `windows/lab/src/runner.rs` `tests` (construct a minimal `RunResult`/`Row` with a `MetricValue::Number(210.2242156495795)` following the existing test builders in this file; assert the rendered CSV contains `hornvale_kernel::quantize(210.2242156495795).to_string()` and not the raw literal). If an existing `render_csv` test already builds a `RunResult`, extend it instead of duplicating the builder.

- [ ] **Step 2: Run to verify it fails** — `cargo test -p hornvale-lab render_csv` (or the extended test's name) — Expected: FAIL (raw value present).

- [ ] **Step 3: Quantize the numeric cell** in `render_csv`:

```rust
MetricValue::Number(n) => {
    csv_content.push_str(&format!("{}", hornvale_kernel::quantize(*n)))
}
```

- [ ] **Step 4: Run tests** — `cargo test -p hornvale-lab` — Expected: PASS. (`load_rows` round-trips whatever is written; quantized values parse back exactly.)

- [ ] **Step 5: Regenerate both census fixtures.**

```bash
cargo run --release -p hornvale -- lab run studies/census-lands-drift.study.json
cargo run --release -p hornvale -- lab run studies/census-of-the-meeting.study.json
git status book/src/laboratory/   # confirm rows.csv (+ summaries/charts) updated
```

- [ ] **Step 6: Verify calibration still loads the fixture cleanly** — `cargo test -p hornvale-lab` and the windows/lab calibration test (the ~145s census loads `rows.csv` as a drift-checked fixture; it must match the regenerated file). Expected: PASS.

- [ ] **Step 7: Commit** — `git add windows/lab/ book/src/laboratory/ && git commit -m "feat(lab): quantize CSV numbers; re-freeze census fixtures (save-format epoch)"`

---

### Task 4: Quantize scene + ephemeris JSON

**Files:**
- Modify: `windows/scene/src/lib.rs` (annotate every `f64` / `Option<f64>` / `Vec<f64>` field on the artifact structs — `TilesScene`, `Feature`, `SystemScene`, `StarScene`, `PlanetScene`, `MoonScene`, and the ephemeris struct — with `#[serde(serialize_with = ...)]`)
- Modify: `windows/scene/tests/golden.rs` if it embeds expected byte strings
- Regenerate: `book/src/gallery/scene-tiles-seed-42.json`, `book/src/gallery/scene-system-seed-42.json`, `clients/orrery/testdata/ephemeris-seed-42.json`

**Interfaces:**
- Consumes: `hornvale_kernel::quantize_serde::{f64_field, opt_f64_field, vec_f64_field}`.

- [ ] **Step 1: Write the failing test** in `windows/scene/src/lib.rs` `tests`: build the seed-42 tiles scene, serialize it, and assert the serialized JSON contains no substring matching a ≥12-fractional-digit float (a cheap proxy for "quantized"). Concretely: serialize, then assert every number token matches `QUANTIZE_SIG_DIGITS`-or-fewer significant digits — simplest form: `assert!(!serde_json::to_string(&scene).unwrap().split(|c: char| !(c.is_ascii_digit() || c=='.')).any(|t| t.split('.').nth(1).is_some_and(|frac| frac.len() > 11)))`. Prefer reusing the crate's existing scene builder + a seeded world helper from `golden.rs`.

- [ ] **Step 2: Run to verify it fails** — `cargo test -p hornvale-scene` — Expected: FAIL (raw floats present).

- [ ] **Step 3: Annotate the artifact struct fields.** For each field:
  - `f64`: `#[serde(serialize_with = "hornvale_kernel::quantize_serde::f64_field")]`
  - `Option<f64>`: `#[serde(serialize_with = "hornvale_kernel::quantize_serde::opt_f64_field")]`
  - `Vec<f64>`: `#[serde(serialize_with = "hornvale_kernel::quantize_serde::vec_f64_field")]`

  Apply to at least: `TilesScene::{sea_level_m, elevation_m, unrest}`, `Feature::{latitude, longitude}`, `StarScene::{luminosity_rel, hz_inner_au, hz_outer_au}`, `PlanetScene::{orbit_au, year_days, day_length_days (Option), obliquity_deg, year_phase_offset}`, `MoonScene::{sidereal_days, phase_offset, distance_mm, size_rel}`, and the ephemeris struct's f64 fields. Grep the file for every `pub .*: f64`, `Option<f64>`, `Vec<f64>` to be exhaustive; missing one leaves a drift hole.

- [ ] **Step 4: Run tests** — `cargo test -p hornvale-scene` — Expected: PASS. Fix `golden.rs` expected strings if it hard-codes serialized bytes (regenerate them from the new output).

- [ ] **Step 5: Regenerate the scene + ephemeris artifacts.**

```bash
cargo run -p hornvale -- new --seed 42 --out /tmp/hv-sky.json
cargo run -p hornvale -- scene tiles  --world /tmp/hv-sky.json > book/src/gallery/scene-tiles-seed-42.json
cargo run -p hornvale -- scene system --world /tmp/hv-sky.json > book/src/gallery/scene-system-seed-42.json
cargo run -p hornvale-scene --example ephemeris_golden > clients/orrery/testdata/ephemeris-seed-42.json
```

- [ ] **Step 6: Commit** — `git add windows/scene/ book/src/gallery/scene-*.json clients/orrery/testdata/ && git commit -m "feat(scene): quantize scene + ephemeris JSON; re-freeze artifacts (save-format epoch)"`

---

### Task 5: Full artifact regeneration + gate

**Files:** every committed artifact (regenerate via the CI list), plus any golden/pinned float updated by the epoch.

- [ ] **Step 1: Run the full CI artifact regeneration list** verbatim from `.github/workflows/ci.yml` "Artifacts are current" step (first_light, three almanacs, concepts/streams/phonology/dictionary dumps, all PNG maps + their `.md`, scene tiles/system, both lab censuses in `--release`, ephemeris). Then `git status` to see everything that moved.

- [ ] **Step 2: Confirm the safe artifacts did NOT move** — the three almanacs, `elevation-seed-42.md`, `dictionary-generated.md`, and the reference dumps should be unchanged (they carry no high-precision floats). If one moved, investigate before proceeding — it means a coarse value straddled a display boundary under the epoch, which is expected-and-fine to commit, but confirm it is only a formatting-level change.

- [ ] **Step 3: Handle the PNG maps.** Regenerate elevation/biome/settlement/paleo/star-chart PNGs (Step 1 did). If any PNG changed, it is because a pixel bucket shifted under the epoch — commit the regenerated PNG. If a PNG shows spurious churn (timestamp/metadata), confirm the encoder is deterministic (it must already be, since these were drift-checked before).

- [ ] **Step 4: Re-pin any remaining golden float values.** Search the workspace for tests that hard-code expected floats derived from worldgen (`grep -rnE 'assert.*[0-9]\.[0-9]{6,}' --include=*.rs`); update any that the epoch shifted, in THIS change (per the re-baseline-golden-pins rule — do not defer). Run the property batteries: `cargo test -p hornvale-astronomy --test genesis_properties && cargo test -p hornvale-terrain --test tectonic_properties`.

- [ ] **Step 5: Type-audit.** `cargo run --manifest-path tools/type-audit/Cargo.toml -- check` then regenerate the report `... -- report > docs/audits/type-audit-report.md`. Fix any new untagged `pub` primitives (the new `quantize` fns are tagged in Task 1).

- [ ] **Step 6: The full gate.**

```bash
cargo test --workspace 2>&1 | tee /tmp/hv-test.txt
cargo fmt --check
cargo clippy --workspace --all-targets -- -D warnings
git diff --exit-code book/src/gallery/ book/src/reference/ book/src/laboratory/ clients/orrery/testdata/ docs/audits/
```
Expected: all pass; the final `git diff --exit-code` is clean (all artifacts committed).

- [ ] **Step 7: Commit** any remaining regenerated artifacts / re-pinned goldens — `git add -A && git commit -m "chore: re-freeze all committed artifacts under the quantization epoch"`

---

### Task 6: Decision log, chronicle, book freshness, retrospective (Definition of Done)

**Files:**
- Create: `docs/decisions/0027-serialized-floats-are-quantized-for-cross-platform-determinism.md`
- Create: `book/src/chronicle/<n>-<slug>.md` (next chronicle number; add to chronicle index/SUMMARY)
- Create: `docs/retrospectives/<campaign>-<slug>.md`
- Modify: `CLAUDE.md` determinism section (add the cross-platform quantization contract), `docs/README.md` if the decision changes the map, `book/src/open-questions.md` if this resolves/moves a Confidence-Gradient bet (re-score it).

- [ ] **Step 1: Decision entry `0027`.** Record: the problem (platform libm divergence in committed goldens), the rejected alternatives (platform-specific artifact sets — fragile against runner-image glibc bumps and O(platforms) maintenance; a portable `libm` crate — violates the no-new-deps rule), the chosen mechanism (quantize to `QUANTIZE_SIG_DIGITS` significant digits at each serialization boundary, libm-free), the epoch nature, and the known residual risk (a future seed's value can straddle a quantization boundary; CI drift catches it and the fixture is re-pinned — the golden-file lifecycle). **Record the Lorenz guard-rail:** quantization is safe *only because* reload re-derives providers from the lossless `u64` seed (`terrain_of`/`astronomy` call `generate(world.seed, …)` at full precision) — the quantized ledger is a posterior record, never dynamical state. Dynamical resumption must always re-derive from the seed; **never seed a chaotic forward-integrator from quantized ledger floats.** If a chaotic subsystem ever needs true checkpoint/resume, it gets its own full-precision (or seed-plus-input-schedule) format, distinct from the published save. Also add this guard-rail to the CLAUDE.md determinism section. Confirm `0027` does not collide with concurrent WIP before merge (check `main` numbering + any foreign branches).

- [ ] **Step 2: Chronicle entry** narrating the campaign at the book's altitude (technical, mathematical, comprehensible without the code): why byte-identity held within a platform but not across, and how quantization at the boundary restores it.

- [ ] **Step 3: Book freshness sweep** — update any chapter that asserts "byte-identical worlds" without the cross-platform nuance; add the quantization contract where determinism is described.

- [ ] **Step 4: Retrospective** (one page, process not product): the value of measuring the drift surface (grepping high-precision floats per artifact) before choosing a choke point; the trap of assuming "amd64 is one bucket."

- [ ] **Step 5: Final gate + commit** — re-run the full gate (Task 5 Step 6), `mdbook build book` to confirm the book builds, then commit the docs.

---

## Self-Review notes

- **Precision choice (`QUANTIZE_SIG_DIGITS = 10`)** is the one real judgment call. It kills 15–16-digit ULP noise with ~5–6 digits of margin and preserves far more precision than a text sim observes. If the regenerated seed-42 fixture shows any boundary sensitivity (rare), lowering to 8 widens the margin. Documented as a save-format constant.
- **Residual risk (accepted):** quantization is ~99.97% effective, not a proof of cross-platform identity — a value landing within sub-quantum distance of a rounding boundary can still flip. This is caught by the same CI drift checks and resolved by re-pinning, exactly like any golden. Full source-level identity would require a portable libm (rejected: no-new-deps).
- **Coarse artifacts untouched:** almanacs/maps/dictionary/reference dumps carry no high-precision floats; their producers are deliberately not modified.
- **Debug==release preserved:** `quantize` is opt-level-independent (format/parse), so `release_determinism` still holds.
