//! Seed-derivation stream labels for tectonic genesis. Labels are permanent
//! save-format contracts; deliberate regeneration uses an epoch suffix
//! (`terrain/plate-count/v2`), never a rename.

/// Root stream: every terrain chain hangs off `world_seed.derive(ROOT)`.
pub const ROOT: &str = "terrain";
/// Plate count draw.
pub const PLATE_COUNT: &str = "plate-count";
/// Per-plate seed-position draws (two `next_f64` per plate, sequential).
pub const PLATE_SEEDS: &str = "plate-seeds";
/// Continental-fraction draw, then one continental roll per plate.
pub const PLATE_KIND: &str = "plate-kind";
/// Per-plate Euler pole draws (axis then rate, sequential).
pub const PLATE_MOTION: &str = "plate-motion";
/// Per-plate orogenic maturity draws.
pub const MATURITY: &str = "maturity";
/// Hotspot count, then per-hotspot position and strength draws.
pub const HOTSPOTS: &str = "hotspots";
/// Target ocean fraction draw.
pub const OCEAN_FRACTION: &str = "ocean-fraction";
