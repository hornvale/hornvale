//! The field pack (The Weft, Task 4): materializes [`GeneratedTerrain`]'s
//! per-vertex macro-state accessors into `VertexMap<f64>`s, once per world.
//!
//! `LocaleContext::blend_at` (`windows/locale`) takes a `&VertexMap<f64>`,
//! but macro state is exposed only as per-vertex accessor *calls*
//! (`drainage_at`, `crust_age_at`, `boundary_distance_at`, `material_at`).
//! Each scalar a derived-feature kind reads has to be materialized once as a
//! `VertexMap<f64>` and passed in — the shape
//! `predator_pressure_from(wc, terrain, report) -> VertexMap<f64>` already
//! uses. This module is that materialization for terrain's continuous macro
//! causes.
//!
//! **Only continuous causes, never categorical labels.** `Biome` is an
//! `enum`, and a category cannot be bilinearly blended — `blend_at`
//! structurally cannot smooth a label, which is *why* the biome monotony
//! defect this campaign addresses exists. `MaterialBuffer` is not
//! categorical: five continuous `[0,1]` scalars (`silica`, `grain`,
//! `induration`, `carbonate`, `metamorphic_grade`), so rock is blendable, and
//! `carbonate` is the karst driver that physically governs springs and caves
//! (spec §5.3).
//!
//! **Scope: a starting set, not a complete one.** These are the
//! terrain-sourced causes needed so far — `carbonate`/`drainage` for
//! spring/seep — plus `land` (Task 7, R1 — see below), and the pack is
//! expected to grow as more kinds are built. Spec §5.6's rows also name
//! `elevation` (spring/seep), `slope` (overhang/hollow), and thicket/brake
//! needs `moisture`/`temperature`; none of those are packed yet, because no
//! kind reads them yet — added when the kind that needs it is built, not
//! packed speculatively ahead of a consumer. `crust_age_at`/
//! `boundary_distance_at` remain plain [`GeneratedTerrain`] accessors for the
//! same reason.
//!
//! **Never add a `productivity` field.** `LocaleContext` computes it
//! blend-then-combine: blend temperature, blend moisture, *then* apply a
//! Liebig minimum. A materialized `productivity` `VertexMap` would compute
//! combine-then-blend instead, and because the minimum is non-linear,
//! `blend(f(a, b)) != f(blend(a), blend(b))` — not a last-bit rounding
//! difference, a different quantity wearing the right name. A kind that needs
//! productivity (thicket/brake) reads `temperature`/`moisture` fields
//! directly and combines *after* blending, the same order `LocaleContext`
//! itself uses.
//!
//! **`land` closes the eligibility defect (Task 7, R1).** Review of Task 5
//! measured that 59% of all spring occurrences (seed 42, every walk-depth
//! facet over all 40,962 vertices) landed on facets with literally no macro
//! cause, including open ocean — the lerp's macro-independent floor
//! (`(1-contextuality) * noise`) is real and unconditional, so nothing
//! stopped a spring from surfacing mid-ocean. `GeneratedTerrain::cave_at`
//! already states the fix's shape: `if self.is_ocean(id) { return None; }`,
//! an early ground test *before* any noise is drawn. `land` materializes
//! that same `is_ocean` read as a blendable `[0,1]` flag (`1.0` land, `0.0`
//! ocean) rather than exposing a typed `ReferenceElevation`/`sea_level` pair
//! as bare `f64`s — the `elevation-convention` waiver The Datum campaign
//! retired for exactly that datum is not reopened here. See
//! `crate::weft::kinds::land_eligible` for the blended threshold test.

use hornvale_kernel::{Vertex, VertexMap};
use hornvale_terrain::GeneratedTerrain;

/// Macro-state scalars materialized once per world as `VertexMap<f64>`s, so
/// `windows/locale`'s `LocaleContext::blend_at` (which takes a
/// `&VertexMap<f64>`) can bilinearly blend them at any walk facet. Built by
/// [`field_pack_from`]; every field is total over `terrain.geosphere()`'s
/// vertices and in the range documented on the `GeneratedTerrain` accessor it
/// materializes.
/// type-audit: bare-ok(ratio: carbonate), bare-ok(ratio: induration), bare-ok(count: drainage), bare-ok(ratio: land)
pub struct FieldPack {
    /// Carbonate content, `[0,1]`
    /// (`GeneratedTerrain::material_at(v).carbonate`) — the karst driver
    /// spring/seep and overhang/hollow are diagnostic of (spec §5.3, §5.6).
    pub carbonate: VertexMap<f64>,
    /// Induration/hardness, `[0,1]` (`GeneratedTerrain::material_at(v)
    /// .induration`, agreeing with `GeneratedTerrain::induration_at`
    /// everywhere) — governs where an overhang can hold its own roof.
    pub induration: VertexMap<f64>,
    /// Flow-accumulation drainage: upstream land-vertex count, `>= 0`, `0` on
    /// ocean (`GeneratedTerrain::drainage_at`). A count, not `[0,1]`-scaled —
    /// a water-source signal for spring/seep.
    pub drainage: VertexMap<f64>,
    /// Ground eligibility, `1.0` land / `0.0` ocean
    /// (`!GeneratedTerrain::is_ocean(v)` as a blendable flag) — every kind's
    /// shared ground test (Task 7, R1; see this module's own doc and
    /// `crate::weft::kinds::land_eligible`).
    pub land: VertexMap<f64>,
}

/// Materialize [`FieldPack`] from `terrain`, one pass over
/// `terrain.geosphere()`'s vertices per field — the `predator_pressure_from`
/// shape. Pure read of already-generated macro state: no seed, byte-identical
/// across calls for the same `terrain`.
pub fn field_pack_from(terrain: &GeneratedTerrain) -> FieldPack {
    let geo = terrain.geosphere();
    FieldPack {
        carbonate: VertexMap::from_fn(geo, |v: Vertex| terrain.material_at(v).carbonate),
        induration: VertexMap::from_fn(geo, |v: Vertex| terrain.material_at(v).induration),
        drainage: VertexMap::from_fn(geo, |v: Vertex| terrain.drainage_at(v)),
        land: VertexMap::from_fn(geo, |v: Vertex| if terrain.is_ocean(v) { 0.0 } else { 1.0 }),
    }
}
