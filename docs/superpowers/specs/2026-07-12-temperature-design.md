# Temperature — the kernel temperature family (Kernel Units family #2)

**Status:** Draft · **Date:** 2026-07-12 · **Author:** Nathan (with Claude)
**Doctrine:** [`docs/design/kernel-units-doctrine.md`](../../design/kernel-units-doctrine.md)
**Follows:** The Datum (elevation) — the pilot that proves the mechanism.

## Summary

The second family in the kernel units series (doctrine §Roadmap). Unlike The
Datum, the *type* is already designed and battle-tested — `paleoclimate` grew it
independently: `Celsius` (`Sub → TempAnomaly`, `Add<TempAnomaly> → Celsius`, **no**
`Celsius + Celsius`) and `TempAnomaly`, both citing decision 0008. That is
exactly the doctrine's *ratio-but-intensive → carries a delta* pattern, built
before the doctrine named it. This campaign **promotes that proven pair to the
kernel** as `Temperature`/`TempAnomaly`, then closes the one gap: **climate is
still bare `f64`**, and `worldgen` bridges it by wrapping climate's output in
`paleoclimate::Celsius` — a cross-domain type borrow that exists only for lack of
a shared home. Byte-identical migration.

**Anchor (doctrine's guardrail):** this dedups `paleoclimate::Celsius`/
`TempAnomaly` into the kernel and removes worldgen's borrow — a *live* wart, not
a speculative build.

## Background: what temperature looks like today

- **`paleoclimate` is already typed.** `domains/paleoclimate/src/units.rs` defines
  `Celsius(f64)` and `TempAnomaly(f64)`; `strata.rs` uses `CellMap<Celsius>`. The
  `Celsius`/`TempAnomaly` boundary is guarded (an anomaly is producible *only* by
  `Celsius` subtraction or `TempAnomaly::from_offset_c`).
- **`climate` is bare `f64`.** `mean_temperature → CellMap<f64>`,
  `temperature_at → f64`, `mean_temperature_at → f64`; `biome`'s `temp_c`/`sst_c`
  and `habitability`'s `temp_c` are bare (`pending(wave-2)`).
- **`worldgen` bridges** — `Celsius::new(climate.mean_temperature_at(c))` and the
  era-offset flow via `Celsius`'s `Add`. It imports `paleoclimate::Celsius`
  because that is the only temperature type in the tree.

Climate cannot use `paleoclimate::Celsius` (domains depend on the kernel and
nothing else). Promoting to the kernel is precisely what lets climate speak the
type at all — the doctrine's placement rule, with a live anchor.

## Classification (per doctrine)

- **Multiplicity: singular.** One temperature quantity, comparable everywhere →
  one shared kernel type.
- **Scale / additivity: ratio but *intensive*.** Absolute temperature has a true
  zero (ratio), but you do not add two temperatures → no `Add<Temperature>`;
  `Temperature − Temperature → TempAnomaly` and `Temperature ± TempAnomaly`. The
  delta (`TempAnomaly`) is the ratio-extensive companion, and it **crosses
  boundaries** (deep-time era vs present), so — unlike elevation's bare `Sub`
  delta — it is a first-class named type (already so in paleoclimate).
- **Cyclicity: linear. Rank: scalar.** No special machinery.

**Canonical representation: Celsius (forced).** The doctrine flags Kelvin
(ratio, no datum) vs Celsius (smaller magnitude → better quantization precision)
as a live tradeoff — but for *this* migration byte-identity **forces Celsius**:
every temperature the tree *emits* (into artifacts, and into the ledger as a bare
`Value::Number`) is already Celsius. Kelvin-canonical would change those emitted
numbers → not behavior-free.
Precision also favors Celsius. So the kernel type is canonical-Celsius, with a
`.kelvin()` accessor for physics/ratios. (A future Kelvin-canonical world would
be a non-byte-identical epoch — out of scope.)

## What this campaign builds

### 1. Promote to `kernel/src/units.rs`

Move `Celsius`/`TempAnomaly` from `paleoclimate` into the kernel, renaming the
absolute type to the **semantic** `Temperature` (doctrine: the unit is a
representation, not the type identity — a `.celsius()`/`.kelvin()` accessor, not
a `Celsius` type, so a future canonical change need not rename it):

- `Temperature(f64)` — canonical degrees Celsius. `new` mirrors the *existing*
  `Celsius::new` validation exactly (behavior-free; an absolute-zero floor is a
  possible later tightening, not here). `get()`/`.celsius()` (raw), `.kelvin()`
  (accessor). `Sub → TempAnomaly`, `Add<TempAnomaly> → Temperature`; **no**
  `Add<Temperature>`.
- `TempAnomaly(f64)` — the ratio-extensive delta; `from_offset_c` guarded
  constructor preserved; `Add<TempAnomaly>` legal.
- **No `serde`** — temperature is compute-only (never serialized; it crosses to
  the ledger/artifacts as a bare `f64` via `.get()`). Keep the moved type's
  derived `PartialOrd`.

Register/re-export from `kernel/src/lib.rs`. (Assumes `units.rs` exists from The
Datum; if Temperature lands first, it creates the module.)

### 2. Re-point `paleoclimate`

Replace `paleoclimate`'s local `Celsius`/`TempAnomaly` with the kernel types
(`use hornvale_kernel::{Temperature, TempAnomaly}`); delete them from its
`units.rs` (keep `IceVolume`/`SeaLevelChange` — single-domain). `CellMap<Celsius>`
→ `CellMap<Temperature>`; the guarded-anomaly invariant moves with the type.

### 3. Type `climate`'s boundary (the gap)

`mean_temperature → CellMap<Temperature>`; `temperature_at`, `mean_temperature_at`
→ `Temperature`; `biome`'s `temp_c`/`sst_c` and `habitability`'s `temp_c` →
`Temperature`. Construction wraps the existing Celsius `f64`; comparisons/deltas
use `Sub`/`PartialOrd`.

### 4. Drop the bridge in `worldgen` + `scene`; re-type `locale`

`Celsius::new(climate.mean_temperature_at(c))` → `climate.mean_temperature_at(c)`
(already `Temperature`); re-point imports to the kernel. Scene and `locale` emit
`.get()` (raw Celsius) at their JSON boundaries — same bytes; `locale` blends
`mean_temperature_at` into its serialized `temperature_c`.

### 5. Retag + regenerate the type audit

Climate's `pending(wave-2: temp_c|sst_c|temperature_c)` become newtype boundaries;
regenerate `docs/audits/type-audit-report.md`.

## Determinism and byte-identity

Temperature is **compute-only — never serialized** (no `serde`; it reaches the
ledger and artifacts as a bare `f64` via `.get()`, the doctrine's trace boundary).
No stream labels, seed-derivation, consumption order, or physics formulas change,
and the canonical Celsius values are unchanged. **Guard test:** the seed-42
almanac and lab CSV regenerate **byte-identical** (the emit boundary prints the
same numbers). The rename `Celsius → Temperature` is source-level only.

## Rollout staging (for the plan)

Origin-out, each step compiling:
1. Kernel `Temperature`/`TempAnomaly` (+ module if absent). Additive.
2. `paleoclimate` re-points to the kernel; delete its local copies.
3. `climate` boundaries typed to `Temperature`.
4. `worldgen` + `scene` + `locale` drop the bridge / re-point imports.
   *(Coordinate with The Lab Performance campaign.)*
5. Retag + regenerate audit.
6. Byte-identity guard + full workspace gate.

## Testing

- **Byte-identity** — seed-42 world/almanac identical pre/post.
- **Unit** (moved with the type) — anomaly producible only via `Sub`/`from_offset_c`;
  `Add<TempAnomaly>` round-trips; `.kelvin()` == `.celsius()` + 273.15; `new`
  matches prior `Celsius::new`.
- **Type audit** `check` passes; report drift-clean.
- **Full gate.**

## Sequencing

After The Datum (shares `units.rs`); kernel is free (The Room Mesh shipped
2026-07-12); worldgen stage coordinates with The Lab Performance campaign. main's CI is green
again (libm, decision 0041), so this is executable now.

## Follow-ons

- **`.fahrenheit()`** accessor — when a presentation frame needs it (reactive).
- **Absolute-zero floor** in `Temperature::new` — a validation tightening (safe
  only if physics never emits below −273.15 °C); deliberately *not* in this
  behavior-free migration.
- **Kelvin-canonical** — only as a future non-byte-identical epoch, if ever.

## Definition of Done

Per CLAUDE.md: chronicle entry, book freshness sweep, retrospective, and a
Confidence-Gradient re-score if the typed-quantities bet moves.
