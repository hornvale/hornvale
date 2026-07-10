//! Seed-derivation labels for astronomy (permanent contracts, spec §3).

/// Root stream label for astronomy.
/// type-audit: bare-ok(identifier-text)
pub const ROOT: &str = "astronomy";
/// Star mass draw.
/// type-audit: bare-ok(identifier-text)
pub const STAR_MASS: &str = "star-mass";
/// Anchor mass draw.
/// type-audit: bare-ok(identifier-text)
pub const ANCHOR_MASS: &str = "anchor-mass";
/// Rotation regime/period draw.
/// type-audit: bare-ok(identifier-text)
pub const ROTATION: &str = "rotation";
/// Anchor orbital-distance draw.
/// type-audit: bare-ok(identifier-text)
pub const ORBIT: &str = "orbit";
/// Obliquity draw.
/// type-audit: bare-ok(identifier-text)
pub const OBLIQUITY: &str = "obliquity";
/// Moon count draw.
/// type-audit: bare-ok(identifier-text)
pub const MOON_COUNT: &str = "moon-count";
/// Per-moon parameter draws (one stream reused across attempts).
/// type-audit: bare-ok(identifier-text)
pub const MOONS: &str = "moons";
/// Neighborhood draws.
/// type-audit: bare-ok(identifier-text)
pub const NEIGHBORS: &str = "neighbors";
/// Deep-time orbital-forcing draws (eccentricity + obliquity oscillation + precession).
/// type-audit: bare-ok(identifier-text)
pub const FORCING: &str = "forcing";
/// Per-body genesis phase offsets (year, day, and each moon).
/// type-audit: bare-ok(identifier-text)
pub const PHASE_OFFSETS: &str = "phase-offsets";
/// Per-neighbor celestial position draws (declination, right ascension).
/// type-audit: bare-ok(identifier-text)
pub const NEIGHBOR_POSITIONS: &str = "neighbor-positions";
