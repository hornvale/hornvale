//! Seed-derivation labels for the climate domain (save-format contract — a
//! rename silently corrupts every world; deliberate regeneration uses an epoch
//! suffix, e.g. `.../v2`). Climate is otherwise seed-free (temperature,
//! moisture, and biome are pure derived reads); the only stochastic climate
//! layer is drawn weather (The Firmament).

/// The label deriving the drifting weather-phase noise seed (The Firmament).
/// type-audit: bare-ok(identifier-text)
pub const WEATHER_PHASE: &str = "climate/weather/phase/v1";
