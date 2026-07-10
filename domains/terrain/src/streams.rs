//! Seed-derivation stream labels for tectonic genesis. Labels are permanent
//! save-format contracts; deliberate regeneration uses an epoch suffix
//! (`terrain/plate-count/v2`), never a rename.

/// Root stream: every terrain chain hangs off `world_seed.derive(ROOT)`.
/// type-audit: bare-ok(identifier-text)
pub const ROOT: &str = "terrain";
/// Plate count draw.
/// type-audit: bare-ok(identifier-text)
pub const PLATE_COUNT: &str = "plate-count";
/// Per-plate seed-position draws (two `next_f64` per plate, sequential).
/// type-audit: bare-ok(identifier-text)
pub const PLATE_SEEDS: &str = "plate-seeds";
/// Continental-fraction draw, then one continental roll per plate.
/// type-audit: bare-ok(identifier-text)
pub const PLATE_KIND: &str = "plate-kind";
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
/// Craton draws: continental-area budget, craton count, then per-craton
/// center (two draws), radius, age — sequential. Draw counts are a
/// save-format contract.
/// type-audit: bare-ok(identifier-text)
pub const CRATONS: &str = "cratons";
/// Per-plate Voronoi weight draws (heavy-tailed; one per plate).
/// type-audit: bare-ok(identifier-text)
pub const PLATE_WEIGHTS: &str = "plate-weights";
/// Plate-edge noise. Hash-noise only — never consumed as a `Stream`.
/// type-audit: bare-ok(identifier-text)
pub const PLATE_EDGE: &str = "plate-edge";
