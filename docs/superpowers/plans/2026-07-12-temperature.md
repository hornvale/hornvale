# Temperature Implementation Plan

> **Status: EXECUTED (2026-07-13) and merged.** All five tasks landed
> byte-identical; `Temperature`/`TempAnomaly` are kernel types, climate's
> boundary is typed, and worldgen's `Celsius` bridge is gone. See the
> chronicle (`book/src/chronicle/temperature.md`) and the retrospective
> (`docs/retrospectives/temperature.md`).

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Promote `paleoclimate`'s `Celsius`/`TempAnomaly` pair to the kernel as `Temperature`/`TempAnomaly`, type `climate`'s bare-`f64` temperature boundary, and remove `worldgen`'s cross-domain `Celsius` borrow — a byte-identical migration.

**Architecture:** The absolute-temperature type already exists and is battle-tested in `paleoclimate` (`Celsius`: `Sub → TempAnomaly`, `Add<TempAnomaly>`, no `Celsius + Celsius`; `TempAnomaly` guarded). This campaign *moves* it to the kernel (so `climate` — which can only depend on the kernel — can speak it), renames the absolute type to the semantic `Temperature`, and closes the one gap where `climate` still emits bare `f64`. Canonical representation is **Celsius** (every stored temperature already is; byte-identity forbids a Kelvin swap), with a `.kelvin()` accessor.

**Tech Stack:** Rust edition 2024, `std` + `serde` only. `hornvale_kernel::{CellMap, Geosphere, CellId}`.

## Global Constraints

- **Follows The Datum.** Shares `kernel/src/units.rs`. If The Datum has landed, *add* to that module; if not, Task 1 creates it. Kernel is free (The Room Mesh shipped 2026-07-12); **run `make preflight` + absorb `main` before Task 1**, and re-confirm `paleoclimate/src/units.rs` still defines `Celsius`/`TempAnomaly` with the `Sub`/`Add` impls this plan moves.
- **Worldgen stage coordinates with The Lab Performance campaign** (it owns `windows/worldgen/`).
- Determinism is constitutional: no `HashMap`/`HashSet`; no wall-clock; float ordering via `total_cmp`. **Byte-identical migration** — same seed → identical worlds, almanacs, artifacts.
- Canonical unit is **Celsius**; `get()` returns raw Celsius degrees (matching the current `Celsius::get`); `.kelvin()` is an accessor (`get() + 273.15`). Temperature is **compute-only — never serialized**; it reaches the ledger/artifacts as a bare `f64` (`.get()`). Byte-identity is therefore an **artifact** property (almanac/lab print identical Celsius numbers); the source-level rename changes nothing emitted.
- `#![warn(missing_docs)]`; one-line doc on every public item.
- Commit gate: **`make gate`** (fmt + clippy + `cargo nextest run --workspace` + doctests; heavy live-worldgen tier `#[ignore]`d, in `make gate-full`); iterate with `make gate-fast`. CLAUDE.md's gate ladder is authoritative.
- Every commit message ends with: `Claude-Session: https://claude.ai/code/session_01VADHoYci3kwUe14WhmCDKJ`

---

### Task 1: Kernel `Temperature` + `TempAnomaly`

**Files:**
- Modify: `kernel/src/units.rs` (create if The Datum hasn't; add these types)
- Modify: `kernel/src/lib.rs` (re-export)
- Test: inline `#[cfg(test)] mod tests`

**Interfaces:**
- Produces: `hornvale_kernel::{Temperature, TempAnomaly}`. `Temperature`: `new(f64) -> Result<Self, UnitError>` (raw Celsius; validation identical to the moved `Celsius::new`), `get(self) -> f64` (raw Celsius), `kelvin(self) -> f64`, `impl Sub for Temperature { type Output = TempAnomaly; }`, `impl Add<TempAnomaly> for Temperature { type Output = Temperature; }`. `TempAnomaly`: `from_offset_c(f64) -> TempAnomaly` (now **`pub`** — see note), `get(self) -> f64`, `impl Add for TempAnomaly`. Both derive `Debug, Clone, Copy, PartialEq, PartialOrd` (compute-only; not serialized).

- [ ] **Step 1: Write the failing tests**

Add to `kernel/src/units.rs`:

```rust
#[cfg(test)]
mod temperature_tests {
    use super::*;

    #[test]
    fn get_is_raw_celsius_and_kelvin_offsets() {
        let t = Temperature::new(25.0).unwrap();
        assert_eq!(t.get(), 25.0);
        assert_eq!(t.kelvin(), 25.0 + 273.15);
    }

    #[test]
    fn difference_of_two_temperatures_is_an_anomaly() {
        let warm = Temperature::new(20.0).unwrap();
        let cool = Temperature::new(5.0).unwrap();
        let delta: TempAnomaly = warm - cool;
        assert_eq!(delta.get(), 15.0);
    }

    #[test]
    fn temperature_plus_anomaly_round_trips() {
        let base = Temperature::new(10.0).unwrap();
        let a = TempAnomaly::from_offset_c(-4.0);
        assert_eq!((base + a).get(), 6.0);
    }

    #[test]
    fn anomalies_add() {
        let a = TempAnomaly::from_offset_c(3.0);
        let b = TempAnomaly::from_offset_c(-1.0);
        assert_eq!((a + b).get(), 2.0);
    }
}
```

- [ ] **Step 2: Run to verify failure**

Run: `cargo test -p hornvale-kernel --lib temperature_tests`
Expected: FAIL — `Temperature`/`TempAnomaly` not found.

- [ ] **Step 3: Move the types from `paleoclimate`, rename, add `kelvin()`**

Cut `Celsius` and `TempAnomaly` (their structs, impls, and `Sub`/`Add`/`from_offset_c`) **verbatim** from `domains/paleoclimate/src/units.rs` into `kernel/src/units.rs`. Then:
- Rename `Celsius` → `Temperature` throughout the moved block (type name, doc comments, `Self`).
- Add the accessor and derives:

```rust
impl Temperature {
    /// This reading in kelvin (`get()` is the canonical raw degrees Celsius).
    /// type-audit: bare-ok(constructor-edge: return)
    pub fn kelvin(self) -> f64 {
        self.get() + 273.15
    }
}
```

- Keep the existing derives **verbatim** (`Debug, Clone, Copy, PartialEq, PartialOrd`). Temperature is **compute-only — never serialized** (it reaches the ledger/artifacts as a bare `f64` via `.get()`, the doctrine's trace boundary), so do **not** add `serde`.
- **`TempAnomaly::from_offset_c` becomes `pub`** (was `pub(crate)` in paleoclimate). Note in its doc: the kernel is a shared home, so the guarded-offset constructor is now crate-public; the finite validation is preserved, and `Sub` remains the only *other* production path. (This loosens the "only paleoclimate fabricates" guarantee — a deliberate, behavior-free relaxation for a shared type.)

Register in `kernel/src/lib.rs`: `pub use units::{Temperature, TempAnomaly};` (and `pub mod units;` if not already present from The Datum).

- [ ] **Step 4: Run to verify pass**

Run: `cargo test -p hornvale-kernel --lib temperature_tests`
Expected: PASS (4 tests).

- [ ] **Step 5: Gate and commit**

Run: `cargo fmt -p hornvale-kernel && cargo clippy -p hornvale-kernel --all-targets -- -D warnings && cargo test -p hornvale-kernel`

```bash
git add kernel/src/units.rs kernel/src/lib.rs
git commit -m "feat(kernel): Temperature/TempAnomaly — shared temperature family (Kernel Units)

Claude-Session: https://claude.ai/code/session_01VADHoYci3kwUe14WhmCDKJ"
```

---

### Task 2: Re-point `paleoclimate` to the kernel

**Files (modify):** `domains/paleoclimate/src/units.rs` (delete moved types), `lib.rs` (re-export), `strata.rs`, `ice.rs`.

**Interfaces:**
- Consumes: `hornvale_kernel::{Temperature, TempAnomaly}`.
- Produces: `paleoclimate` uses the kernel types; its `units.rs` keeps only `IceVolume`/`SeaLevelChange`/`UnitError` (single-domain). `strata.rs`: `CellMap<Temperature>`, `freeze: Temperature`.

- [ ] **Step 1: Delete the moved types; import from the kernel**

In `domains/paleoclimate/src/units.rs` remove the now-moved `Celsius`/`TempAnomaly` blocks. In `lib.rs` change the re-export to `pub use hornvale_kernel::{Temperature, TempAnomaly};` (keep the local `IceVolume`, `SeaLevelChange`).

- [ ] **Step 2: Rename `Celsius` → `Temperature` at every use**

Run: `grep -rn "Celsius" domains/paleoclimate/src` and replace each `Celsius` with `Temperature` (`strata.rs`: `temperature: &CellMap<Temperature>`, `freeze: Temperature`, the `Celsius::new(...)` test constructions; `ice.rs` doc references). `TempAnomaly` uses are unchanged.

- [ ] **Step 3: Build and test**

Run: `cargo test -p hornvale-paleoclimate`
Expected: PASS — the guarded-anomaly tests and strata behavior are identical (same values, renamed type).

- [ ] **Step 4: Gate and commit**

Run: `cargo fmt -p hornvale-paleoclimate && cargo clippy -p hornvale-paleoclimate --all-targets -- -D warnings`

```bash
git add domains/paleoclimate/src
git commit -m "refactor(paleoclimate): use kernel Temperature/TempAnomaly (dedup)

Claude-Session: https://claude.ai/code/session_01VADHoYci3kwUe14WhmCDKJ"
```

---

### Task 3: Type `climate`'s temperature boundary (the gap)

**Files (modify):** `domains/climate/src/{temperature,provider,biome,habitability}.rs` (+ compiler-surfaced).

**Interfaces:**
- Consumes: `hornvale_kernel::Temperature`.
- Produces: `mean_temperature -> CellMap<Temperature>`; `temperature_at -> Temperature`; `mean_temperature_at(&self, CellId) -> Temperature`; `biome`'s `temp_c`/`sst_c` and `habitability`'s `temp_c` become `Temperature`.

- [ ] **Step 1: Enumerate**

Run: `grep -rnE "-> f64|CellMap<f64>|temp_c|sst_c|mean_temp|temperature_at" domains/climate/src`
Identify the temperature-typed positions (temperatures only — leave `moisture`, `elevation`, `sea_level` alone; those are other families).

- [ ] **Step 2: Convert the boundaries; wrap at construction**

Change the signatures above to `Temperature` / `CellMap<Temperature>`. At the point `mean_temperature` builds its map, wrap the existing Celsius `f64` expression: `Temperature::new(<expr>).expect("temperature is finite")`. Comparisons/differences use `Sub`/`PartialOrd`. Follow `cargo build -p hornvale-climate` for the rest.

- [ ] **Step 3: Test — behavior unchanged**

Run: `cargo test -p hornvale-climate`
Expected: PASS (`temperature_falls_with_latitude_on_a_spinning_world` etc. — identical values).

- [ ] **Step 4: Gate and commit**

Run: `cargo fmt -p hornvale-climate && cargo clippy -p hornvale-climate --all-targets -- -D warnings`

```bash
git add domains/climate/src
git commit -m "refactor(climate): temperature boundary is kernel Temperature (behavior-free)

Claude-Session: https://claude.ai/code/session_01VADHoYci3kwUe14WhmCDKJ"
```

---

### Task 4: Drop the bridge in `worldgen` + `scene`; re-type `locale`

**Files (modify):** `windows/worldgen/src/lib.rs`, `windows/scene/src/lib.rs`, `windows/locale/src/lib.rs`. `locale` (shipped P2 campaign 2) blends `climate.mean_temperature_at` into its serialized `temperature_c` (bare `f64`). **Coordinate with The Lab Performance campaign first.**

**Interfaces:**
- Consumes: the typed boundaries from Tasks 2–3.
- Produces: worldgen imports `Temperature`/`TempAnomaly` from the kernel; the `Celsius::new(climate.mean_temperature_at(c))` wrap becomes a direct `climate.mean_temperature_at(c)`; scene and `locale` emit `.get()` (raw Celsius) at their temperature JSON boundaries.

- [ ] **Step 1: Enumerate**

Run: `grep -rnE "Celsius|TempAnomaly|mean_temperature|temperature" windows/worldgen/src windows/scene/src windows/locale/src`

- [ ] **Step 2: Re-point imports; drop the wrap**

Change `hornvale_paleoclimate::{Celsius, ...}` imports to `hornvale_kernel::{Temperature, TempAnomaly}` (rename `Celsius` → `Temperature`). Remove the redundant `Temperature::new(...)` wrap where climate now returns `Temperature`. Scene: `.get()` at the emit boundary keeps the same bytes. Follow the compiler.

- [ ] **Step 3: Test**

Run: `cargo test -p hornvale-worldgen -p hornvale-scene -p hornvale-locale`
Expected: PASS.

- [ ] **Step 4: Gate and commit**

Run: `cargo fmt -p hornvale-worldgen -p hornvale-scene -p hornvale-locale && cargo clippy -p hornvale-worldgen -p hornvale-scene -p hornvale-locale --all-targets -- -D warnings`

```bash
git add windows/worldgen/src windows/scene/src windows/locale/src
git commit -m "refactor(worldgen,scene,locale): use kernel Temperature; drop the Celsius bridge

Claude-Session: https://claude.ai/code/session_01VADHoYci3kwUe14WhmCDKJ"
```

---

### Task 5: Prove byte-identity + retag the type audit

**Files (modify):** climate temperature `type-audit:` tags; `docs/audits/type-audit-report.md` (regenerated).

- [ ] **Step 1: Prove behavior-free — regenerate artifacts, assert no drift**

```bash
SKIP_CENSUS=1 bash scripts/regenerate-artifacts.sh
git diff --exit-code book/
```

Expected: **empty diff** — Celsius-canonical means every emitted temperature number
is unchanged. (Censuses are never regenerated locally; they refresh once per
campaign on the AWS box via `scripts/aws-gate/regen-git.sh`, just before the
merge to main, with warning to Nathan.)

- [ ] **Step 2: Retag + regenerate the audit report**

Flip climate's `pending(wave-2: temp_c|sst_c|temperature_c)` on the now-typed boundaries (they are newtypes, not bare primitives). Then:

```bash
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md
```

Expected: `check` passes; report regenerated.

- [ ] **Step 3: Full gate and commit**

Run: `make gate` (fmt + clippy + nextest + doctests)

```bash
git add -A
git commit -m "chore(type-audit): type climate temperatures; prove byte-identity (Temperature)

Claude-Session: https://claude.ai/code/session_01VADHoYci3kwUe14WhmCDKJ"
```

---

## Definition of Done (campaign close — use the closing-a-campaign skill)

- **Chronicle entry** — `book/src/chronicle/temperature.md`.
- **Book freshness sweep** — any chapter describing climate temperature as bare `f64`; the type-audit reference page.
- **Retrospective** — `docs/retrospectives/temperature.md`.

## Self-review notes

- **Spec coverage:** kernel `Temperature`/`TempAnomaly` → Task 1; dedup paleoclimate → Task 2; type climate's gap → Task 3; drop worldgen bridge → Task 4; byte-identity + retag → Task 5. Canonical-Celsius / `.kelvin()` accessor → Task 1 + Global Constraints. Follow-ons (`.fahrenheit()`, absolute-zero floor, Kelvin-canonical epoch) correctly *not* tasked.
- **Type consistency:** `Temperature::{new, get, kelvin}`, `Sub → TempAnomaly`, `Add<TempAnomaly>`, `TempAnomaly::{from_offset_c, get}` used identically in Tasks 2–4 as defined in Task 1.
- **Deliberate API change:** `TempAnomaly::from_offset_c` widens `pub(crate)` → `pub` on promotion (behavior-free; noted in Task 1).
- **Drift caveat:** Tasks 2–4 use `grep` + the compiler rather than line numbers (executes after The Datum and possibly Lab Performance churn).
