//! The field pack (The Weft, Task 4): materializes [`GeneratedTerrain`]'s
//! (and, since Task 7, [`GeneratedClimate`]'s) per-vertex macro-state
//! accessors into `VertexMap<f64>`s, once per world.
//!
//! `LocaleContext::blend_at` (`windows/locale`) takes a `&VertexMap<f64>`,
//! but macro state is exposed only as per-vertex accessor *calls*
//! (`drainage_at`, `crust_age_at`, `boundary_distance_at`, `material_at`,
//! `mean_temperature_at`, `moisture_at`). Each scalar a derived-feature kind
//! reads has to be materialized once as a `VertexMap<f64>` and passed in —
//! the shape `predator_pressure_from(wc, terrain, report) -> VertexMap<f64>`
//! already uses. This module is that materialization for terrain's and
//! climate's continuous macro causes.
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
//! **Scope grew with Task 7, on a needs-a-consumer basis, exactly as this
//! doc previously said it would.** `carbonate`/`drainage` fed spring/seep
//! (Task 5); `induration`/`slope` now feed overhang/hollow,
//! `temperature`/`moisture` feed thicket/brake, and `land` feeds every
//! kind's ground-eligibility test (Task 7, R1 — see below). Nothing here is
//! packed speculatively ahead of a consumer; `crust_age_at`/
//! `boundary_distance_at` remain plain [`GeneratedTerrain`] accessors for the
//! same reason no kind reads them through this pack.
//!
//! **`slope` is [`hornvale_terrain::local_slope`], promoted rather than
//! reimplemented (Task 7, controller ruling R4).** `local_slope` already
//! feeds `domains/terrain/src/channel.rs`'s committed confinement/floodplain
//! geometry; a worldgen-side reformulation of "how steep is it here" would
//! be a second implementation of a formula the world already depends on —
//! the exact shape this campaign has closed three times before (the blend
//! promoted to the kernel, `SphereFbm`, `room_edge`). Materialized here as
//! the raw signed metres-of-fall-per-radian value `local_slope` returns
//! (unbounded, occasionally negative); a kind's own macro-state recipe does
//! its own unit conversion into `[0,1]`, the same division of labour
//! `drainage`/`spring_macro_state` already established (`kinds.rs`'s
//! `SPRING_DRAINAGE_SATURATION`).
//!
//! **Never add a `productivity` field.** `LocaleContext` computes it
//! blend-then-combine: blend temperature, blend moisture, *then* apply a
//! Liebig minimum. A materialized `productivity` `VertexMap` would compute
//! combine-then-blend instead, and because the minimum is non-linear,
//! `blend(f(a, b)) != f(blend(a), blend(b))` — not a last-bit rounding
//! difference, a different quantity wearing the right name. Thicket/brake
//! (the kind that needs productivity) reads `temperature`/`moisture` fields
//! directly and combines *after* blending, the same order `LocaleContext`
//! itself uses — see `windows/worldgen/src/weft/kinds.rs`'s
//! `thicket_macro_state`, which is its own implementation rather than a
//! call into `windows/locale` (worldgen may not depend on a window it
//! composes; `crate::weft`'s own module doc states the same layering
//! constraint for `LocaleContext` itself).
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
//! `crate::weft::kinds::land_eligible` for the blended threshold test every
//! kind currently shares.

use hornvale_climate::GeneratedClimate;
use hornvale_kernel::{Vertex, VertexMap};
use hornvale_terrain::GeneratedTerrain;

/// Macro-state scalars materialized once per world as `VertexMap<f64>`s, so
/// `windows/locale`'s `LocaleContext::blend_at` (which takes a
/// `&VertexMap<f64>`) can bilinearly blend them at any walk facet. Built by
/// [`field_pack_from`]; every field is total over `terrain.geosphere()`'s
/// vertices and in the range documented on the accessor it materializes.
/// type-audit: bare-ok(ratio: carbonate), bare-ok(ratio: induration), bare-ok(count: drainage), pending(wave-2: slope), pending(wave-2: temperature), bare-ok(ratio: moisture), bare-ok(ratio: land)
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
    /// Local gradient, metres of fall per radian toward the vertex's own
    /// downhill target (`hornvale_terrain::local_slope`, promoted from
    /// `domains/terrain/src/channel.rs`; see this module's own doc). `0.0`
    /// at a terminal sink. Unbounded and occasionally negative — a raw
    /// physical rate, not `[0,1]`-scaled — overhang/hollow's own recipe
    /// saturates it (`kinds.rs`'s `OVERHANG_SLOPE_SATURATION`).
    pub slope: VertexMap<f64>,
    /// Annual-mean temperature, °C (`GeneratedClimate::mean_temperature_at`,
    /// never the seasonal `temperature_at(vertex, at)` — see
    /// `crate::weft::kinds`'s own doc for why a derived feature must be a
    /// pure function of place, not of observation time). Unbounded and
    /// signed — thicket/brake's own Liebig response saturates it.
    pub temperature: VertexMap<f64>,
    /// Moisture, `[0,1]` (`GeneratedClimate::moisture_at`, "climate's
    /// dimensionless moisture field" in its own doc) — the second half of
    /// thicket/brake's productivity read.
    pub moisture: VertexMap<f64>,
    /// Ground eligibility, `1.0` land / `0.0` ocean
    /// (`!GeneratedTerrain::is_ocean(v)` as a blendable flag) — every kind's
    /// shared ground test (Task 7, R1; see this module's own doc and
    /// `crate::weft::kinds::land_eligible`).
    pub land: VertexMap<f64>,
}

/// Materialize [`FieldPack`] from `terrain` and `climate`, one pass over
/// `terrain.geosphere()`'s vertices per field — the `predator_pressure_from`
/// shape. `climate` must share `terrain`'s own geosphere (true of any
/// `climate_of`/`climate_from` build over that same `terrain`, the
/// `terrain_of`/`sky_of` construction-site pattern every caller already
/// uses). Pure read of already-generated macro state: no seed, byte-identical
/// across calls for the same `(terrain, climate)`.
pub fn field_pack_from(terrain: &GeneratedTerrain, climate: &GeneratedClimate) -> FieldPack {
    let geo = terrain.geosphere();
    let globe = terrain.globe();
    FieldPack {
        carbonate: VertexMap::from_fn(geo, |v: Vertex| terrain.material_at(v).carbonate),
        induration: VertexMap::from_fn(geo, |v: Vertex| terrain.material_at(v).induration),
        drainage: VertexMap::from_fn(geo, |v: Vertex| terrain.drainage_at(v)),
        slope: VertexMap::from_fn(geo, |v: Vertex| {
            hornvale_terrain::local_slope(globe, geo, v)
        }),
        temperature: VertexMap::from_fn(geo, |v: Vertex| climate.mean_temperature_at(v).get()),
        moisture: VertexMap::from_fn(geo, |v: Vertex| climate.moisture_at(v)),
        land: VertexMap::from_fn(geo, |v: Vertex| if terrain.is_ocean(v) { 0.0 } else { 1.0 }),
    }
}
