//! Seed-derivation labels for the climate domain (save-format contract — a
//! rename silently corrupts every world; deliberate regeneration uses an epoch
//! suffix, e.g. `.../v2`). Climate is otherwise seed-free (temperature,
//! moisture, and biome are pure derived reads); the only stochastic climate
//! layer is drawn weather (The Firmament).

hornvale_kernel::stream_labels! {
    /// The label deriving the drifting weather-phase noise seed (The Firmament).
    WEATHER_PHASE = "climate/weather/phase/v1" => "drifting weather-phase noise seed (The Firmament)";
    /// The label deriving a cell's characteristic variant — what a settlement
    /// there is named for (The Toponym). Distinct from the per-room prose draw.
    VARIANT_CELL = "climate/variant/cell/v1" => "the characteristic variant of a cell (The Toponym)";
}
