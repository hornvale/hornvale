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
/// Spin-direction draw: prograde or retrograde (SKY-22).
/// type-audit: bare-ok(identifier-text)
pub const SPIN_DIRECTION: &str = "spin-direction";
/// Per-moon orbital-inclination draws (SKY-6).
/// type-audit: bare-ok(identifier-text)
pub const MOON_INCLINATIONS: &str = "moon-inclinations";
/// Wanderer count draw.
/// type-audit: bare-ok(identifier-text)
pub const WANDERER_COUNT: &str = "wanderer-count";
/// Per-wanderer parameter draws, sequential (no attempts loop —
/// `generate_wanderers` draws each wanderer's parameters once, in order).
/// type-audit: bare-ok(identifier-text)
pub const WANDERERS: &str = "wanderers";
/// Background starfield draws: count, then per-star position/brightness (derived catalog — consumed on demand, never in genesis).
/// type-audit: bare-ok(identifier-text)
pub const STARFIELD: &str = "starfield";
