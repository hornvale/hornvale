//! Seed-derivation labels for astronomy (permanent contracts, spec §3).

/// Root stream label for astronomy.
pub const ROOT: &str = "astronomy";
/// Star mass draw.
pub const STAR_MASS: &str = "star-mass";
/// Anchor mass draw.
pub const ANCHOR_MASS: &str = "anchor-mass";
/// Rotation regime/period draw.
pub const ROTATION: &str = "rotation";
/// Anchor orbital-distance draw.
pub const ORBIT: &str = "orbit";
/// Obliquity draw.
pub const OBLIQUITY: &str = "obliquity";
/// Moon count draw.
pub const MOON_COUNT: &str = "moon-count";
/// Per-moon parameter draws (one stream reused across attempts).
pub const MOONS: &str = "moons";
/// Neighborhood draws.
pub const NEIGHBORS: &str = "neighbors";
/// Deep-time orbital-forcing draws (eccentricity + obliquity oscillation + precession).
pub const FORCING: &str = "forcing";
/// Per-body genesis phase offsets (year, day, and each moon).
pub const PHASE_OFFSETS: &str = "phase-offsets";
/// Per-neighbor celestial position draws (declination, right ascension).
pub const NEIGHBOR_POSITIONS: &str = "neighbor-positions";
