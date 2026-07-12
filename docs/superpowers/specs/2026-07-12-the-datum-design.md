# The Datum — shared units in the kernel (the elevation/length family)

**Status:** Draft · **Date:** 2026-07-12 · **Author:** Nathan (with Claude)
**Campaign:** The Datum · **Sequenced after:** The Room Mesh (P2 kernel substrate)

## Summary

Decision 0008 ratified that coherent physical quantities crossing API
boundaries are hand-rolled newtypes — but said nothing about **where those
newtypes live**. Because a domain crate depends on `hornvale-kernel` and
nothing else (never another domain), a quantity spoken by *more than one*
domain has exactly one legal home: the kernel. Today the pervasive
elevation/sea-level datum dodges this by staying bare `f64` under the
`elevation-convention` waiver — a pragmatic wart, not a considered end-state,
and one whose "convention" has no durable home in `docs/decisions/` (it is
only referenced from Campaign-3 plans).

This campaign does two things:

1. **Establishes the policy** (a ratified decision): *coherent physical
   quantities that cross domain boundaries live in the kernel; single-domain
   quantities stay in their domain.* This supersedes 0008's silence on
   location and gives the `elevation-convention` waiver a cited home plus a
   scheduled retirement.
2. **Proves the policy end-to-end on one family** — the elevation/length
   datum — by introducing a kernel `ReferenceElevation` newtype, re-typing the
   elevation field and its sea-level threshold across every crate that speaks
   them, and retiring the `elevation-convention` waiver for that datum.

**Scope discipline.** One family, proven completely. Angle
(latitude/longitude, obliquity, `radius_rad`), temperature (`Celsius`), and
migrating the existing domain-local unit types (astronomy's `Degrees`,
paleoclimate's `Celsius`) onto a kernel core are **named follow-on waves, not
designed here** (see Follow-ons).

## Why now, and why a spec rather than code

The honest implementation of a typed elevation datum is unavoidably a **kernel
change** (the shared type + a `CellMap` element-type change) **plus a ripple
through the composition root** (`hornvale-worldgen`) and `windows/scene`. That
lands on two crates under active surgery: the kernel (The Room Mesh) and
worldgen (The Lab Performance campaign). Running a second big kernel semantic
rewrite *in parallel* with The Room Mesh is precisely the
two-simultaneous-keystone-rewrites collision the project's parallel-campaign
guidance flags as manual-judgment-required.

**Design, however, is conflict-free.** This spec (and the implementation plan
that follows it) can be written now without touching contended code.
**Implementation is sequenced after The Room Mesh frees the kernel**, and its
worldgen stage coordinates with The Lab Performance campaign. The Datum is The
Room Mesh's successor.

## Background: what "elevation" actually means today

The code does **not** store height-above-sea-level, and it does **not** store
radius-from-core. `domains/terrain/src/elevation.rs` derives elevation by
isostasy:

```
isostatic_elevation_m = ISOSTASY_M_PER_KM * (thickness_km - ISOSTASY_REF_KM)
                      = 180.0 * (thickness_km - 30.0)
```

So **0 m is a reference-thickness (30 km) crust floating at equilibrium** — a
height against the *crust-flotation reference datum*. Sea level is then
*derived from* the elevation field, not a datum for it:
`derive_sea_level(elevation, ocean_fraction)` sorts the field and returns the
value at the `ocean_fraction` percentile. Consequences already relied upon:

- **No-sea worlds need no special case.** `ocean_fraction = 0` → sea level is
  the field minimum (nothing floods); `→ 1.0` → the maximum (a waterworld).
  Sea level is always a well-defined value *on the same axis as elevation* —
  never physically absent, only degenerate. No `Option` is required.
- **The datum is planet-independent by definition.** 0 m means the same
  physical thing on every world. The ambiguity Nathan flagged — "elevation
  means different things on different planets" — is therefore fixed by *naming
  the datum*, not by switching representation to radius-from-core. The
  sea-level-relative frame is the one that varies per world; the stored one
  does not.

## The mental model (recorded so it does not drift)

Every vertical measure is one physical quantity — a radial position, in
meters — measured against a **datum** (a *horizon*: a surface sitting at some
radius). A named "elevation" is always `point − horizon`, signed by direction.
Horizons come in two kinds, and the split *is* the bug-safety story:

```
+-------------+-------------------------------+-------------------+---------------+
| Kind        | Defined by                    | Moves at rate of  | Role          |
+-------------+-------------------------------+-------------------+---------------+
| PRIMARY     | geometry of the body          | never (invariant) | canonical     |
|             | (core, mean radius, geoid)    |                   | reference     |
+-------------+-------------------------------+-------------------+---------------+
| DERIVED     | a statistic/threshold over a  | the field it sums | recompute     |
|             | field (isostatic ref, sea     |  (see rate ladder)| from field;   |
|             | level, Moho, snow line, ...)  |                   | never store   |
|             |                               |                   | *against*     |
+-------------+-------------------------------+-------------------+---------------+
```

**Rate ladder** (predictability of a derived horizon → whether a reframing
against it is safe to cache): invariant → tectonic (10^6–10^8 yr: isostatic
reference, Moho, surface min/max) → epochal (10^4–10^5 yr: sea level,
permafrost) → seasonal (snow line, water table) → instantaneous (tide, surge).
Anything stored *against* a non-invariant horizon is a latent "the number
changed but the rock didn't" bug (a mountain's stored elevation must not move
when an ice age lowers the sea).

The current stored quantity sits at the **tectonic** rung (isostatic
reference) — planet-independent and stable except under tectonics/erosion,
which is exactly when a point's elevation *should* change. That is why it is a
sound canonical store, and why this campaign preserves it byte-for-byte rather
than re-deriving to radius-from-core.

### Three type roles (only the first is built here)

- **`ReferenceElevation`** — the canonical stored value: meters against the
  isostatic reference datum. What lives in `CellMap` and serializes. **Built
  in this campaign.**
- **`RelativeElevation<H>`** — a signed reframing against a named datum `H`
  (positive above, negative below). This is the surveyor's *reduced level* and
  the driller's *subsea depth*: one signed axis per datum, never split into
  separate "height"/"depth" types. Direction is the sign; the `<H>` parameter
  guards against the real bug — mixing two *different* datums (sea-level- vs
  cave-relative). **Deferred** until a second datum exists (see Follow-ons).
- **`GeocentricRadius`** — the from-core derived view
  (`R_planet + reference_elevation`). Named honestly (geodesy's *geocentric
  radius*), not "absolute." **Deferred** until a consumer needs it.

Naming rationale: "elevation" is reserved for datum-relative measures (its
ordinary meaning); the from-core quantity is a *radius*, not an elevation;
"Absolute" is rejected because it misreads as `|elevation|`. `Reference`
names the datum without leaking the isostatic *mechanism* that most call sites
do not care about.

## The policy this campaign pilots

The full **kernel units doctrine** — placement, the proactive stance, the
Stevens-based promotion classifier (scale/multiplicity/cyclicity/rank), derived
units, and the trace / presentation / evolution boundaries — lives in
[`docs/design/kernel-units-doctrine.md`](../../design/kernel-units-doctrine.md),
to be ratified as **decision 0027** when the successor "Kernel Units" campaign
lands. The Datum is that doctrine's pilot; only the elevation-specific
application is recorded here:

> The kernel `ReferenceElevation` type gives the `elevation-convention` waiver a
> cited home and retires it for the elevation datum — an *interval* quantity, so
> it carries its datum (the isostatic reference) per the doctrine's
> "interval types carry their datum" rule. Sibling conventions
> (`crust-km-convention`) follow the same path in their own waves.

The datum mental model above (primary/derived horizons, the rate ladder) is this
pilot's reasoning; the doctrine records the general classification.

## What this campaign builds

### 1. `kernel/src/units.rs` (new) — `ReferenceElevation`

```
/// Metres of elevation relative to the isostatic reference datum
/// (0 m = a reference-thickness crust floating at equilibrium).
/// Planet-independent: 0 m means the same physical thing on every world.
/// This is NOT height above sea level — sea level is itself a value of this
/// type, derived from the elevation field. Deep ocean floor is strongly
/// negative; validation admits any finite value, either sign.
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Serialize, Deserialize)]
#[serde(transparent)]
pub struct ReferenceElevation(f64);
```

(`Serialize`/`Deserialize` are required for `#[serde(transparent)]` to take
effect — without them the attribute is inert.)

- `new(value: f64) -> Result<Self, UnitError>` — rejects non-finite only (no
  range clamp; sign is meaningful).
- `get(self) -> f64`.
- A `total_cmp`-based ordering helper, used for inherent `min`/`max` **and**
  for the field sort in `derive_sea_level` (`elevation.iter()` sorted by value)
  — deterministic (removes `f64::min`/`max`/`sort` NaN ambiguity) and
  consistent with the workspace `total_cmp` rule.
- `impl Sub<ReferenceElevation> for ReferenceElevation { type Output = f64; }`
  — the difference (metres above/below the other point) is a **local
  intermediate** consumed on the spot (lapse rate, depth shading) and never
  crosses a pub boundary, so per 0008 it stays bare `f64`. If a
  height-above-a-datum ever *does* cross a boundary, it earns
  `RelativeElevation<H>` then, not now.
- `#[serde(transparent)]` so the serialized bytes are identical to the bare
  `f64` they replace. Quantization stays exactly where it is (the emit
  boundary); this type never enters the compute path's precision story.

The `UnitError` type follows the existing per-domain pattern
(`domains/astronomy/src/units.rs`); the kernel gets its own copy (the kernel
depends on nothing).

Register the module in `kernel/src/lib.rs` (`pub mod units;` + re-export).
**This one line is the entire kernel-surface collision with The Room Mesh** —
mechanical, resolved by re-adding the module declaration after that campaign's
`lib.rs` edits land.

### 2. Re-type the datum across the crates that speak it

`CellMap<T>` is already generic (`from_fn(geo, |c| ...)`), so the storage
change is `CellMap<f64>` → `CellMap<ReferenceElevation>` with no kernel
container surgery. Scalar thresholds `sea_level: f64` → `sea_level:
ReferenceElevation`. Ripple surface (origin → consumers):

- **terrain** (origin): `globe.elevation`, `sea_level`, `derive_sea_level`'s
  return, and internal comparisons/subtractions in `elevation.rs`,
  `shape.rs`, `globe.rs`, `drainage.rs`, `provider.rs`, `render.rs`.
- **climate**, **paleoclimate** (receive the field + threshold):
  `temperature.rs`, `moisture.rs`, `habitability.rs`, `biome.rs`,
  `provider.rs`; `strata.rs`. `paleoclimate::SeaLevelChange` (a deep-time
  **anomaly/delta**) is untouched — only the absolute position is re-typed.
- **worldgen** (composition root, wiring), **windows/scene** (re-emits
  `elevation_at`).

### 3. Retag the type audit

The elevation-datum tags flip from `waiver(elevation-convention: ...)` and
`pending(wave-2: elevation|sea_level|...)` to the newtype verdict
(`bare-ok(constructor-edge: ...)` for `new()`/`get()` edges; the wrapped
values are no longer bare at the boundary). Regenerate
`docs/audits/type-audit-report.md`. (This regenerated artifact is also touched
by The Room Mesh — a guaranteed conflict on that file, resolved by
re-running `type-audit report`, never hand-merged.)

## Determinism and save-format

- **Behavior-free migration.** `#[serde(transparent)]` guarantees identical
  serialized bytes. No stream labels, seed-derivation labels, stream
  consumption order, or physics formulas change. This is a pure representation
  change, exactly like the 0008 retrofit ("a golden test showed the newtype
  migration was behavior-free").
- **Guard test:** a seed-42 world (and its almanac) serialize **byte-identical**
  before and after the migration. Cross-platform quantization is unaffected.
- `total_cmp`-based `min`/`max` preserves (indeed tightens) deterministic
  ordering; no `f64::min`/`max` remain on the datum.

## Rollout staging (for the implementation plan)

Each step compiles and passes its crate's tests before the next — origin
first, consumers after, so no half-typed boundary ever exists:

1. **Kernel type.** Add `ReferenceElevation` + `units` module + `lib.rs`
   registration. Additive; no consumers yet. *(Coordinate with The Room Mesh
   on `lib.rs`.)*
2. **Terrain.** Re-type `globe.elevation`, `sea_level`, `derive_sea_level`;
   convert internal arithmetic to `Sub`/`min`/`max`/`PartialOrd`.
3. **Climate + paleoclimate.** Re-type the received field and threshold at
   every boundary.
4. **Worldgen + scene.** Wire the typed field through the composition root and
   scene emission. *(Coordinate with The Lab Performance campaign.)*
5. **Retag + regenerate** the type audit; land decision 0027.
6. **Gate:** golden byte-identity test, unit tests, full workspace gate,
   artifact drift check.

## Testing

- **Golden byte-identity** — seed-42 world + almanac serialize identically
  pre/post (the behavior-free proof).
- **Unit** — `ReferenceElevation::new` rejects `NaN`/`±inf`; `min`/`max` match
  `total_cmp`; `Sub` yields the correct signed metre delta; `PartialOrd`
  orders as the bare `f64` did.
- **Type audit** — `check` passes with the new verdicts; regenerated report is
  drift-clean.
- **Full gate** — `cargo test --workspace`, `cargo fmt --check`,
  `cargo clippy --workspace --all-targets -- -D warnings`.

## Follow-ons (out of scope; recorded so the map survives)

Classified per the promotion axes (see The policy):

- **`RelativeElevation<H>`** (plural length, affine) — introduce when a *second*
  datum exists (the subterranean/cave frame). Open sub-choice deferred to then:
  compile-time phantom-tag `H` (zero-cost, prevents datum-mixing, heavier Rust)
  vs distinct named views. Until then the sea-level reframing is a local
  `Sub`-derived `f64` (the unnamed elevation delta).
- **`GeocentricRadius`** (from-core view) — likely pulled first by the
  room-scale 3D placement work (positioning a room needs the point's radius).
- **`SurfaceElevation`** — a named height-above-sea-level type; likely pulled by
  The Walk (describing altitude to a player crosses a pub boundary).
- **Angle family** (plural, **cyclic**) — `latitude`/`longitude` (originate in
  `GeoCoord` → kernel), obliquity, `radius_rad`. **Its own campaign, not a
  re-skin of the linear elevation pattern**: `Longitude` wraps (modular
  arithmetic + normalization + shortest-angular-distance), `Latitude` clamps
  (±90). Astronomy's `Degrees` is a plain validated newtype with no wrap, so it
  needs more than a lift-to-kernel. `GeoCoord`'s bare-`f64` latitude is the
  highest-blast-radius target in the system.
- **Temperature family** (singular; ratio *but intensive* → carries a delta) — a
  kernel temperature type promoted *together with* `TempAnomaly` (the ΔT delta,
  already in paleoclimate). Absolute temperature has a true zero (ratio) yet is
  *intensive* — you don't add two temperatures — so it behaves affine (no `Add`;
  `temp − temp → TempAnomaly`), the same delta pattern as the interval
  quantities. Dedupes paleoclimate's `Celsius` and climate's need for one (today
  worldgen wraps climate's bare output in `paleoclimate::Celsius` — a
  cross-domain type borrow that exists only for lack of a shared home). Canonical
  fork to settle then: **Kelvin** (ratio, true-zero, no datum, physics-natural)
  vs **Celsius** (interval representation, smaller magnitude → better
  quantization precision but carries the freezing offset) — a real tradeoff, not
  a foregone Kelvin.
- **`crust-km-convention`** (plural length) — the sibling bare-datum waiver;
  retire it with its own semantic type (`CrustThickness` — *not* a shared
  `Length` with elevation) in the terrain/crust wave.
- **Vector/tensor quantities** (rank ≥ 1) — position, velocity, gradient, today
  bare `[f64; 3]`. A recognized deferred *category*, not designed here; the
  scalar library is not "the whole library."
- **Conventional/ordinal scales** — faith, reputation, danger, Mohs, Beaufort:
  **not units.** Recorded as an explicit fence so the units library never
  becomes their dumping ground (they are ordered enums / ordinal scales).

## Definition of Done (per CLAUDE.md)

Beyond the code gate: a chronicle entry (`book/src/chronicle/`), a book
freshness sweep, a one-page retrospective (`docs/retrospectives/`), and — since
this resolves the typed-quantities/`elevation-convention` bet — a
re-score of the affected `open-questions.md` chapter (the Confidence Gradient
is re-scored, not frozen).

## Sequencing recap

Design (this spec + its plan): **now, conflict-free.**
Implementation: **after The Room Mesh lands** (kernel freed), worldgen stage
**coordinated with The Lab Performance campaign.** The Datum is The Room Mesh's
successor.
