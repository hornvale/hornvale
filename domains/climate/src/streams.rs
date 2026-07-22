//! Seed-derivation labels for the climate domain (save-format contract — a
//! rename silently corrupts every world; deliberate regeneration uses an epoch
//! suffix, e.g. `.../v2`). Climate is otherwise seed-free (temperature,
//! moisture, and biome are pure derived reads); the only stochastic climate
//! layer is drawn weather (The Firmament).

hornvale_kernel::stream_labels! {
    /// The label deriving the drifting weather-phase noise seed (The Firmament).
    WEATHER_PHASE = "climate/weather/phase/v1" => "drifting weather-phase noise seed (The Firmament)";
}
