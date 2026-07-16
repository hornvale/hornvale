# The Terminator (Regime-Aware Insolation, SKY-24) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make the habitability `insolation` field rotation-regime-aware so tidally-locked worlds reward the terminator (restoring tide-Ambient religion as emergent physics), via a shared `climate::substellar_cosine` helper and a Locked day/night insolation branch — leaving spinning worlds byte-identical.

**Architecture:** `domains/climate` gains one `substellar_cosine` helper + one `SUBSTELLAR` constant; `temperature` and `moisture` drop their private duplicates and call it (byte-identical). `windows/worldgen::substrate_field` threads the `RotationRegime` (already returned by `stellar_inputs`, currently discarded) and, on `Locked` worlds, computes insolation as a substellar cosine flux instead of the latitude-only annual mean. The Niche's `niche_per_species_k` gets the regime plumbed through but its K math is untouched — this fixes its input, not the model. Spec: `docs/superpowers/specs/2026-07-16-the-terminator-design.md` (G3-approved 2026-07-16).

**Tech Stack:** Rust edition 2024, `hornvale-kernel` (`Geosphere`/`CellMap`/`math`), `domains/climate`, `windows/worldgen`, cargo-nextest gate. serde/serde_json only.

## Global Constraints

Copied from the spec and CLAUDE.md; every task's requirements implicitly include these:

- **Save-format change scoped to the Locked regime.** Spinning worlds MUST stay byte-identical (their insolation path is untouched); only tidally-locked worlds re-derive. No terrain-epoch bump. Locked-dependent fixtures/goldens re-pin **in the drifting commit** (Task 3), never deferred. `genesis_is_deterministic` passes at every commit.
- **Byte-identity guards are first-class acceptance, not afterthoughts:** the shared-helper dedupe leaves `temperature` and `moisture` bit-identical (equality tests); the Locked insolation change leaves every SPINNING world bit-identical (a spinning seed's serialized world compares equal pre/post).
- **The Niche is not reopened.** `niche_per_species_k`'s carrying-capacity math is untouched; it only gains a `regime` parameter it forwards to `substrate_field`. No coexistence logic edit.
- **No new pins, no new crates, no `HashMap`/`HashSet`** (`BTreeMap`/`BTreeSet`/`Vec`). No wall-clock. Float sorting via `total_cmp`; per-cell iteration ascending `CellId`. All transcendentals via `hornvale_kernel::math` (libm), never `f64` methods (decision 0041).
- **Quantization at emit boundaries only** — the insolation field computes at full precision.
- Every `pub` item gets a one-line doc comment (`#![warn(missing_docs)]`); every `pub` boundary primitive a `type-audit:` verdict tag.
- **`cargo fmt` is the final step before every commit.** Cost-order: fmt+clippy → scoped `cargo test -p <crate>` → full `make gate` only at the commit points that touch world bytes (Task 3). Run once, capture (`2>&1 | tee /tmp/hv-t.txt`), never re-run to grep.
- **Censuses are NEVER regenerated locally** (decision 0046). Mid-campaign the only tolerated red is stale-census drift on locked-regime seeds; the canonical regen runs once at close on the AWS box (Nathan-authorized), coordinated with rift-and-fit if both near merge.
- **Campaign Autopilot engaged**: G5 auto-continues on green; hard stops for Nathan are the Task-2 probe readout if it demands a roster re-tune (fidelity call), any census/AWS spend, and the G6 close.
- Commit-message trailer (every commit): `Claude-Session: https://claude.ai/code/session_01Rq3ZtUtwfASsKUYdSet8Sx`

---

### Task 1: Shared `substellar_cosine` helper — dedupe temperature & moisture

Extract the substellar geometry into one place so a substellar-driven field can never again forget to be regime-aware. Byte-identical safe refactor; no world bytes move.

**Files:**
- Modify: `domains/climate/src/lib.rs` (re-export the helper) or the module that will own it — put the helper + constant in a small `domains/climate/src/substellar.rs` (new) and `pub use` it, OR in `circulation.rs` beside `RotationRegime` (pick whichever matches the crate's module conventions — check `lib.rs`'s `mod` list; a new `substellar.rs` is cleanest).
- Modify: `domains/climate/src/temperature.rs` (drop private `SUBSTELLAR`/inline cos, call helper).
- Modify: `domains/climate/src/moisture.rs` (same).
- Test: `domains/climate/src/temperature.rs` + `moisture.rs` tests (equality/regression), plus a unit test on the helper.

**Interfaces:**
- Produces:
  - `pub const SUBSTELLAR: [f64; 3] = [1.0, 0.0, 0.0];` — the locked world's substellar point (spec convention: `+x`).
  - `pub fn substellar_cosine(p: [f64; 3]) -> f64` — `p[0]*SUBSTELLAR[0] + p[1]*SUBSTELLAR[1] + p[2]*SUBSTELLAR[2]`, i.e. `dot(p, SUBSTELLAR)`. Doc: cosine of the angle from the substellar point; positive on the day side. type-audit: `bare-ok(ratio: p)`, `bare-ok(ratio: return)`.

- [ ] **Step 1: Write the helper unit test**

```rust
// in the new substellar module's tests
#[test]
fn substellar_cosine_is_one_at_substellar_zero_at_terminator_neg_at_antistellar() {
    assert!((substellar_cosine([1.0, 0.0, 0.0]) - 1.0).abs() < 1e-12);
    assert!(substellar_cosine([0.0, 1.0, 0.0]).abs() < 1e-12);      // terminator meridian
    assert!((substellar_cosine([-1.0, 0.0, 0.0]) + 1.0).abs() < 1e-12);
}
```

- [ ] **Step 2: Run — fails (function absent)**

Run: `cargo test -p hornvale-climate substellar_cosine`
Expected: FAIL — `cannot find function substellar_cosine`.

- [ ] **Step 3: Implement the helper**

```rust
//! The substellar geometry of a tidally-locked world: one home for the
//! `SUBSTELLAR` point and the substellar-cosine every substellar-driven
//! field (temperature, moisture, insolation) shares. Extracting it here
//! makes "a new field forgot to branch on the locked regime" (SKY-24)
//! structurally hard: the geometry has one definition.

/// The substellar point of a locked world (spec convention: `+x`,
/// latitude 0 on the substellar axis).
/// type-audit: bare-ok(ratio)
pub const SUBSTELLAR: [f64; 3] = [1.0, 0.0, 0.0];

/// Cosine of the angle from the substellar point: `dot(p, SUBSTELLAR)`.
/// +1 at the substellar point, 0 on the terminator meridian, -1 at the
/// antistellar point. Positive iff `p` is on the day side.
/// type-audit: bare-ok(ratio: p), bare-ok(ratio: return)
pub fn substellar_cosine(p: [f64; 3]) -> f64 {
    p[0] * SUBSTELLAR[0] + p[1] * SUBSTELLAR[1] + p[2] * SUBSTELLAR[2]
}
```

Register the module in `domains/climate/src/lib.rs` (`mod substellar; pub use substellar::{SUBSTELLAR, substellar_cosine};` — match the crate's existing re-export style).

- [ ] **Step 4: Dedupe temperature.rs** — delete its private `const SUBSTELLAR` (line ~18) and replace the inline `let cos_theta = p[0] * SUBSTELLAR[0] + ...` (line ~68) with `let cos_theta = crate::substellar_cosine(p);`. The arithmetic is identical, so the field is bit-for-bit unchanged.

- [ ] **Step 5: Dedupe moisture.rs** — same: delete its private `const SUBSTELLAR` (line ~18), replace the inline cos (line ~96) with `crate::substellar_cosine(p)`.

- [ ] **Step 6: Add byte-identity regression tests** (if the crate has existing temperature/moisture value tests over seeds, they already guard this; add an explicit one if not)

```rust
#[test]
fn dedupe_preserves_locked_temperature_and_moisture_bytes() {
    // Build a small locked-world climate at a fixed seed/geo the crate's
    // other tests use; assert the temperature and moisture CellMaps equal
    // a committed golden (or, simpler, assert the cos path equals the old
    // inline arithmetic at 200 fibonacci points — see below).
    for p in fib_sphere(200) {
        let inline = p[0] * 1.0 + p[1] * 0.0 + p[2] * 0.0;
        assert_eq!(substellar_cosine(p), inline);
    }
}
```

- [ ] **Step 7: Gate + commit**

```bash
cargo test -p hornvale-climate 2>&1 | tail -3   # temperature + moisture batteries green
cargo fmt && cargo clippy -p hornvale-climate --all-targets -- -D warnings 2>&1 | tail -2
cargo run --manifest-path tools/type-audit/Cargo.toml -- check 2>&1 | tail -2
git add domains/climate/src/
git commit -m "refactor(climate): shared substellar_cosine helper — dedupe temperature+moisture (SKY-24 prep)"
```

### Task 2: Stage-0 synthetic probe — where does dominance peak under the fix?

Read-only instrument (spec §6): on locked seeds, apply the corrected Locked insolation SYNTHETICALLY (no generator change), feed it through the existing niche machinery, and report where the dominant species' K peaks and the resulting presiding sentiment. Its output sets the Task-5 acceptance floor and decides whether Task 4 (roster re-tune) is needed. **No world bytes move; nothing wires into `generate`.**

**Files:**
- Create: `windows/worldgen/tests/insolation_probe.rs` (`#[ignore]`d instrument).
- Create: `book/src/laboratory/the-terminator-probe.md` (the readout).
- Modify: `book/src/SUMMARY.md` (add the page under Laboratory).

**Interfaces:**
- Consumes: `climate::{substellar_cosine, RotationRegime}`, `hornvale_worldgen::{niche_per_species_k, substrate_field, stellar_inputs, default_roster}`, world build via the same path `windows/worldgen`'s own tests use (grep this test dir / `metrics.rs` for the canonical `build`/`FullView` call and mirror it).
- Produces: three probe functions inside the test file (not pub API): `corrected_locked_insolation(geo, insolation_scalar) -> CellMap<f64>` (the §3 Locked formula: day side `insolation_scalar * cos_theta`, night `0.0`), `dominant_k_peak_zone(...)` (substellar/mid/terminator/antistellar by the dominant species' K argmax cell's `substellar_cosine`), and the per-seed table writer.

- [ ] **Step 1: Identify locked seeds.** In the probe, scan world seeds 1..=N (start N=200), build each world's sky, and select those whose `stellar_inputs(&sky).2` is `RotationRegime::Locked`. (Locked is a minority regime; expect a handful per 200. If too few, widen N. Document the count.)

- [ ] **Step 2: Write the instrument**

```rust
//! Stage-0 Terminator probe (spec §6, preregistered): applies the corrected
//! Locked insolation field synthetically on locked seeds and reports where
//! dominant-species carrying capacity peaks, plus the presiding sentiment
//! the correction would yield. Read-only — no generator change. Run by hand:
//! cargo test -p hornvale-worldgen --release --test insolation_probe -- --ignored --nocapture

// For each locked seed:
//  1. Build the world (canonical path).
//  2. Compute corrected_locked_insolation over its geosphere.
//  3. Rebuild the Substrate field with insolation REPLACED by (2) (construct
//     a Substrate CellMap by hand mirroring substrate_field, swapping only
//     the insolation term), run niche_per_species_k's downstream K per
//     species (reuse the pub pieces; if niche_per_species_k takes a
//     Substrate-derived input you cannot inject, compute per-species K the
//     same way it does over the corrected substrate — the goal is the K
//     argmax zone, so replicate the suitability product for the dominant
//     species only).
//  4. Report: dominant species, its K-peak zone (substellar/mid/terminator),
//     and — via the existing religion/sentiment derivation on a world built
//     with the corrected field if feasible, else inferred from the zone —
//     whether the presiding sentiment would be Ambient (tide) or Eternal.
```

The exact injection is the implementer's to write; the fixed contract: corrected insolation = `insolation_scalar * substellar_cosine(p).max(0.0)` (Lambert, night floor 0), locked seeds only, ascending seed order, deterministic. If wiring the full religion derivation under a synthetic field proves too invasive for a read-only probe, the honest fallback is to report the **K-peak zone distribution** (substellar vs terminator) as the proxy — a dominant-K peak in the terminator ring is the mechanism that yields tide-Ambient — and say so.

- [ ] **Step 3: Run (release) and capture**

Run: `cargo test -p hornvale-worldgen --release --test insolation_probe -- --ignored --nocapture 2>&1 | tee /tmp/hv-terminator-probe.txt`
Expected: per-locked-seed table; the fraction of locked seeds whose dominant K peaks in the terminator ring (vs substellar).

- [ ] **Step 4: Write `book/src/laboratory/the-terminator-probe.md`** — the preregistration statement (measured before the generator change), the per-seed table, the terminator-vs-substellar K-peak fraction, and the **verdict**: do the roster's current optima already put dominance in the terminator (→ Task 4 skipped), or do they need re-tuning (→ Task 4 activates, with the probe's sweep naming the target)? Add to `SUMMARY.md`.

- [ ] **Step 5: Gate + commit**

```bash
cargo fmt --check && cargo clippy -p hornvale-worldgen --all-targets -- -D warnings 2>&1 | tail -2
cargo test -p hornvale --test docs_consistency 2>&1 | tail -2
git add windows/worldgen/tests/insolation_probe.rs book/src/laboratory/the-terminator-probe.md book/src/SUMMARY.md
git commit -m "probe(the-terminator): Stage-0 synthetic locked-insolation instrument + readout (spec §6)"
```

**HARD STOP if the probe demands a roster re-tune** (a fidelity call): present Nathan the probe readout and the proposed sweep before Task 4. If the probe shows the optima already terminator-seeking, note it and skip Task 4.

### Task 3: The Locked insolation branch — the save-format commit

Wire the corrected Locked insolation into `substrate_field`, threading the regime through `niche_per_species_k` (plumbing only). Locked worlds re-derive; spinning worlds stay byte-identical; locked-dependent fixtures re-pin here.

**Files:**
- Modify: `windows/worldgen/src/lib.rs` — `substrate_field` (+`regime` param, Locked branch), `niche_per_species_k` (+`regime` param, forwards it), its 2 non-test call sites (lines ~586, ~2418 — pass the `regime` from `stellar_inputs`, which is currently `_regime`), and the test call site (~5086).
- Modify: drifting locked-world fixtures/goldens (Step 4).
- Test: `windows/worldgen` tests — the Locked-branch behavior, the Spinning-byte-identity guard.

**Interfaces:**
- Consumes: `climate::{substellar_cosine, RotationRegime}`.
- Produces:
  - `substrate_field(geo, terrain, climate, obliquity_deg, insolation_scalar, regime: &RotationRegime)` — insolation per cell: `Spinning => annual_mean_insolation(latitude, obliquity_deg, insolation_scalar)` (UNCHANGED); `Locked => insolation_scalar * substellar_cosine(p).max(0.0)`.
  - `niche_per_species_k(geo, terrain, climate, obliquity_deg, insolation_scalar, regime: &RotationRegime, species_set)` — identical body except it forwards `regime` to `substrate_field`. **No K-math change.**

- [ ] **Step 1: Write the Locked-branch test**

```rust
#[test]
fn locked_insolation_peaks_at_substellar_and_zeroes_on_night_side() {
    let geo = Geosphere::new(4);
    // Build a locked-world terrain+climate the file's other tests use;
    // call substrate_field with RotationRegime::Locked.
    let field = substrate_field(&geo, &terrain, &climate, obliquity_deg, insolation_scalar, &RotationRegime::Locked);
    // substellar cell (nearest +x) has the max insolation; an antistellar
    // (nearest -x) cell reads 0.0; a terminator cell reads ~insolation_scalar*|small|.
    let ins = |c| field.get(c).insolation;
    assert!(ins(substellar_cell) > ins(terminator_cell));
    assert_eq!(ins(antistellar_cell), 0.0);
}
```

- [ ] **Step 2: Run — fails** (arity/behavior). Run: `cargo test -p hornvale-worldgen locked_insolation_peaks`. Expected: FAIL (signature mismatch or wrong value).

- [ ] **Step 3: Implement** — add `regime` to both signatures, forward it, and the Locked branch in `substrate_field`:

```rust
insolation: match regime {
    RotationRegime::Spinning { .. } => annual_mean_insolation(
        geo.coord(cell).latitude, obliquity_deg, insolation_scalar,
    ),
    RotationRegime::Locked => {
        let cos_theta = hornvale_climate::substellar_cosine(geo.position(cell));
        insolation_scalar * cos_theta.max(0.0)
    }
},
```

Update the 3 call sites: at lines ~586 and ~2418, change `_regime` to `regime` in the `stellar_inputs` destructure and pass `&regime`; at the test site ~5086 pass an explicit regime.

- [ ] **Step 4: Write the Spinning-byte-identity guard**

```rust
#[test]
fn spinning_worlds_are_byte_identical_across_the_fix() {
    // Pick a KNOWN spinning seed (assert its regime is Spinning first).
    // Build its full world JSON; assert it equals a committed golden that
    // was captured on the PRE-fix code. Simplest robust form: assert the
    // substrate insolation on a spinning world equals annual_mean_insolation
    // per cell (the unchanged path) — i.e. the Spinning arm is literally the
    // old computation.
    for cell in geo.cells() {
        assert_eq!(field.get(cell).insolation,
                   annual_mean_insolation(geo.coord(cell).latitude, obliquity_deg, insolation_scalar));
    }
}
```

- [ ] **Step 5: Run behavior + guard green.** `cargo test -p hornvale-worldgen 2>&1 | tail -5`. Locked branch + spinning guard pass; `genesis_is_deterministic` (wherever it lives) passes. **Fixture tests that build locked worlds now FAIL — that is the save-format change.**

- [ ] **Step 6: Re-pin locked-dependent fixtures in THIS commit.** `SKIP_CENSUS=1 bash scripts/regenerate-artifacts.sh 2>&1 | tail -20` (regenerates committed artifacts; census fixtures stay stale — tolerated). Then `cargo nextest run --workspace --no-fail-fast 2>&1 | tee /tmp/hv-t3.txt | grep -E "FAIL|Summary"`: re-pin every red that is a locked-world golden (world-identity fixtures built on a locked seed, locked almanacs/maps, any calibration exact that moved because locked-world religion changed — e.g. `a_frozen_sky_never_heads_a_cyclic_pantheon`, the ambient/eternal split). Follow each test's own re-pin doc. **Do NOT re-pin any Spinning-world golden — if a spinning fixture drifted, the fix leaked into the spinning path; STOP and fix the code.** The census-fixture reds (schema/value) stay as the tolerated stale-census tier.

- [ ] **Step 7: Full gate + commit**

```bash
cargo fmt && make gate 2>&1 | tail -6   # green except tolerated stale-census
git add -A
git commit -m "feat(worldgen)!: regime-aware insolation — locked worlds reward the terminator (SKY-24, spec §3); spinning worlds byte-identical; locked fixtures re-pinned"
```

**STAGE BOUNDARY** — `make preflight`; absorb main if it moved.

### Task 4: Roster insolation re-tune — ONLY if Task 2 demanded it

Conditional (spec §5). If the Task-2 probe showed the current optima already put dominance in the terminator, **skip this task** (note it in the plan-execution ledger). Build only on measured demand.

**Files (if activated):**
- Modify: `domains/species/src/lib.rs` (the peopled roster's `condition_niche.insolation` optima/width).
- Test: a species-roster test asserting the new optima; re-pin locked fixtures again (same discipline as Task 3 Step 6).

- [ ] **If activated:** set each affected species' insolation optimum from the Task-2 sweep's named target (the value that moves dominant-K into the terminator ring on the worst locked seeds), one commit, locked-fixture re-pins in-commit, `make gate`. Commit: `feat(species): re-tune peopled insolation optima for the corrected locked range (SKY-24, spec §5)`. Document the old→new optima and the sweep in `the-terminator-probe.md`.

### Task 5: Acceptance battery, chronicle, and G6 close

**Files:**
- Create/modify: a calibration battery asserting the preregistered payoff (`windows/lab/tests/` — near the existing `a_frozen_sky_never_heads_a_cyclic_pantheon`).
- Create: `book/src/chronicle/the-terminator.md`; `docs/retrospectives/the-terminator.md`.
- Modify: `book/src/frontier/idea-registry.md` (SKY-24 → shipped, repoint Where at the spec); `book/src/open-questions.md` (re-score the SEQ-1/locked-world bet if it moved); `docs/decisions/` (a decision record for the locked-only save-format scope + the shared-helper guard, if warranted).

- [ ] **Step 1: Acceptance battery** — assert locked-world Ambient recovery to the floor the Task-2 probe set (spec §8): over the drift-study locked seeds, the share heading an Ambient (tide) presiding belief is ≥ that floor (recovered materially off 0). Mirror the existing locked-world calibration's structure. Also assert (or point at Task-3's guard for) Spinning byte-identity.

- [ ] **Step 2: HARD STOP (G6)** — present Nathan the post-G3 ledger digest (save-format/re-pin entries first), the acceptance readout (Ambient recovery achieved vs floor; Spinning byte-identity held), and the canonical census regen authorization request (coordinate with rift-and-fit if both near merge).

- [ ] **Step 3 (post-authorization):** chronicle, retro, SKY-24 status flip, book freshness sweep of the locked-world / SEQ-1 chapters, decision record; `make gate` green.

- [ ] **Step 4:** Close via **closing-a-campaign** (absorb main, keystone refreeze, merge, the AWS regen, push per standing practice).

## Self-review notes (writing-plans checklist)

- Spec coverage: §1→T1(helper)/T3(branch); §2 save-format→globals+T3; §3 correction→T3; §4 shared-helper→T1; §5 roster→T2 probe+T4; §6 probe→T2; §7 touch points→T1/T3; §8 acceptance→T3 guard+T5 battery; §9 non-goals honored (no niche K-math edit, no new pins/crates); §10 staging matches T1–T5.
- Type consistency: `substellar_cosine`/`SUBSTELLAR`/the `regime` param on `substrate_field` and `niche_per_species_k` uniform across T1–T4.
- Conditional task (T4) explicitly gated on T2's measured verdict, per the 0057 banked-on-demand discipline — not a placeholder, a preregistered branch.
- Verify-on-site items flagged inline ("grep the canonical build path", "check the crate's re-export style") are reads, not design gaps.
