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
//! **Scope.** Spec §5.6's four kinds read exactly three terrain-sourced
//! causes between them — `carbonate` and `drainage` (spring/seep) and
//! `induration` (overhang/hollow) — which is exactly [`FieldPack`]'s field
//! set. `crust_age_at`/`boundary_distance_at` remain available on
//! [`GeneratedTerrain`] directly; no kind in this plan reads them, so they
//! are not packed speculatively — a field nothing consumes is untested code.

use hornvale_kernel::{Vertex, VertexMap};
use hornvale_terrain::GeneratedTerrain;

/// Macro-state scalars materialized once per world as `VertexMap<f64>`s, so
/// `windows/locale`'s `LocaleContext::blend_at` (which takes a
/// `&VertexMap<f64>`) can bilinearly blend them at any walk facet. Built by
/// [`field_pack_from`]; every field is total over `terrain.geosphere()`'s
/// vertices and in the range documented on the `GeneratedTerrain` accessor it
/// materializes.
/// type-audit: bare-ok(ratio: carbonate), bare-ok(ratio: induration), bare-ok(count: drainage)
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
    }
}
