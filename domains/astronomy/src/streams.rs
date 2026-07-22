//! Seed-derivation labels for astronomy (permanent contracts, spec §3).
//! Declared via `stream_labels!` (PROC-18), which also generates
//! `stream_labels()` (moved here from `lib.rs`) — a constant and its
//! manifest row can no longer drift apart, so the `ALL_LABELS`
//! cross-check test this file used to carry (added after the STAR_AGE
//! incident, The Reckoning) is retired: there is nothing left for it to
//! catch.

hornvale_kernel::stream_labels! {
    /// Root stream label for astronomy.
    root: ROOT = "astronomy" => "root stream for sky genesis";
    legs {
        /// Star mass draw.
        STAR_MASS = "star-mass" => "main-sequence star mass draw";
        /// Anchor mass draw.
        ANCHOR_MASS = "anchor-mass" => "anchor world mass draw";
        /// Rotation regime/period draw.
        ROTATION = "rotation" => "rotation regime and period draw";
        /// Anchor orbital-distance draw.
        ORBIT = "orbit" => "anchor orbital distance draw";
        /// Obliquity draw.
        OBLIQUITY = "obliquity" => "axial tilt draw";
        /// Moon count draw.
        MOON_COUNT = "moon-count" => "how many moons";
        /// Per-moon parameter draws (one stream reused across attempts).
        MOONS = "moons" => "per-moon mass/distance draws (sequential attempts)";
        /// Neighborhood draws.
        NEIGHBORS = "neighbors" => "neighbor class/distance draws";
        /// Deep-time orbital-forcing draws (eccentricity + obliquity oscillation + precession).
        FORCING = "forcing" => "deep-time orbital forcing";
        /// Per-body genesis phase offsets (year, day, and each moon).
        PHASE_OFFSETS = "phase-offsets" => "per-body genesis phase offsets";
        /// Per-neighbor celestial position draws (declination, right ascension).
        NEIGHBOR_POSITIONS = "neighbor-positions" => "per-neighbor celestial position draws (declination, right ascension)";
        /// Spin-direction draw: prograde or retrograde (SKY-22).
        SPIN_DIRECTION = "spin-direction" => "spin-direction draw: prograde or retrograde";
        /// Per-moon orbital-inclination draws (SKY-6).
        MOON_INCLINATIONS = "moon-inclinations" => "per-moon orbital-inclination draws";
        /// Wanderer count draw.
        WANDERER_COUNT = "wanderer-count" => "how many wandering planets";
        /// Per-wanderer parameter draws, sequential (no attempts loop —
        /// `generate_wanderers` draws each wanderer's parameters once, in order).
        WANDERERS = "wanderers" => "per-wanderer parameter draws, sequential";
        /// Background starfield draws: count, then per-star position/brightness (derived catalog — consumed on demand, never in genesis).
        STARFIELD = "starfield" => "background starfield: count + per-star position/brightness (derived on demand)";
        /// Per-moon ascending-node longitude draws (Eclipse Seasons).
        MOON_NODES = "moon-nodes" => "per-moon ascending-node longitude draws";
        /// Stellar age draw (The Reckoning).
        STAR_AGE = "star-age" => "stellar age draw";
        /// Per-moon formation-mechanism draw (The Reckoning). Drawn after admission
        /// and the distance sort, so count/mass/distance stay byte-identical.
        MOON_FORMATION = "moon-formation" => "per-moon formation-mechanism draw (giant impact vs. capture)";
        /// Per-moon density draw (The Reckoning). Drawn after formation, one draw
        /// per moon in every branch — a `GiantImpact` moon's density is a derived
        /// constant, not a drawn one, but it still consumes a draw so that moon
        /// *i*'s density stream position never depends on how many earlier moons
        /// drew `Capture`.
        MOON_DENSITY = "moon-density" => "per-moon density draw (drawn only for captured moons; impact moons still consume it)";
        /// Per-moon age draw (The Reckoning). Drawn after formation, one draw per
        /// moon in every branch, for the same index-stability reason as
        /// `MOON_DENSITY`.
        MOON_AGE = "moon-age" => "per-moon age draw (impact: coeval jitter under the planet's age; capture: an independent fraction of it)";
    }
}
