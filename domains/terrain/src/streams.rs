//! Seed-derivation stream labels for tectonic genesis. Labels are permanent
//! save-format contracts; deliberate regeneration uses an epoch suffix
//! (`terrain/plate-count/v2`), never a rename.
//!
//! Retired labels (never reuse, never redraw): "plate-kind" (epoch v2 —
//! superseded by "cratons"; Crust spec §9).

/// Root stream: every terrain chain hangs off `world_seed.derive(ROOT)`.
/// type-audit: bare-ok(identifier-text)
pub const ROOT: &str = "terrain";
/// Plate count draw.
/// type-audit: bare-ok(identifier-text)
pub const PLATE_COUNT: &str = "plate-count";
/// Per-plate seed-position draws (two `next_f64` per plate, sequential).
/// type-audit: bare-ok(identifier-text)
pub const PLATE_SEEDS: &str = "plate-seeds";
/// Per-plate Euler pole draws (axis then rate, sequential).
/// type-audit: bare-ok(identifier-text)
pub const PLATE_MOTION: &str = "plate-motion";
/// Per-plate orogenic maturity draws.
/// type-audit: bare-ok(identifier-text)
pub const MATURITY: &str = "maturity";
/// Hotspot count, then per-hotspot position and strength draws.
/// type-audit: bare-ok(identifier-text)
pub const HOTSPOTS: &str = "hotspots";
/// Target ocean fraction draw.
/// type-audit: bare-ok(identifier-text)
pub const OCEAN_FRACTION: &str = "ocean-fraction";
/// Render-lens coastline noise. Hash-noise only — never consumed as a
/// `Stream`; the lens draws nothing (Campaign 25 spec §3).
/// type-audit: bare-ok(identifier-text)
pub const COAST_RENDER: &str = "coast-render";
/// Craton draws: a margin draw (Task 9 iteration 3' reinterpretation —
/// originally an independent area-budget fraction; now scales the
/// ocean-fraction-derived budget instead, same single draw, see
/// `crust::draw_cratons`'s doc), craton count (budget-scaled, no draw of
/// its own), then per-craton center (two draws), radius, age —
/// sequential. Draw counts are a save-format contract.
/// type-audit: bare-ok(identifier-text)
pub const CRATONS: &str = "cratons";
/// Per-plate Voronoi weight draws (heavy-tailed; one per plate).
/// type-audit: bare-ok(identifier-text)
pub const PLATE_WEIGHTS: &str = "plate-weights";
/// Plate-edge noise. Hash-noise only — never consumed as a `Stream`.
/// type-audit: bare-ok(identifier-text)
pub const PLATE_EDGE: &str = "plate-edge";
/// Lithology sub-cell patchiness noise (The Ground, spec §2). Hash-noise
/// only — never consumed as a `Stream`; no draw-order/save-format contract.
/// type-audit: bare-ok(identifier-text)
pub const LITHOLOGY: &str = "lithology";
/// Terrane draws (Sculpting, spec §3): count, then per terrane a host
/// craton index, rim bearing, size, age. A NEW label — existing stream
/// consumption order is untouched (epoch v3 save-format contract).
/// type-audit: bare-ok(identifier-text)
pub const TERRANES: &str = "terranes";
/// Microcontinent draws (Sculpting, spec §3): a NEW label — a fixed
/// candidate count (`crust::MICRO_COUNT_MAX`), then per candidate a
/// position (two draws via `unit_vector`), a radius, and an age,
/// sequential. Every candidate is drawn in full regardless of whether it
/// survives the away-from-majors filter (no rejection loop, no
/// draw-count variance — pin-isolation discipline, mirroring
/// `TERRANES`'s own no-rejection framing for its bearing draws). Existing
/// stream consumption order is untouched (epoch v3 save-format contract).
/// type-audit: bare-ok(identifier-text)
pub const MICROCONTINENTS: &str = "microcontinents";
/// Along-strike arc gating noise (Sculpting, spec §3): gates island-arc
/// and coastal-range edifices into discrete chains instead of continuous
/// walls. Hash-noise only — never consumed as a `Stream`, so it carries
/// no draw-order/save-format contract (see `elevation::boundary_profile_m`).
/// type-audit: bare-ok(identifier-text)
pub const ARC_GATE: &str = "arc-gate";
