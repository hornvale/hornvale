//! Pure Waterworld observation detail, populated in Stage 4.

use crate::waterworld::WaterWorld;

/// Waterworld observation detail.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum WaterWorldDetail {
    /// Whole-world context.
    Planet,
    /// Regional marine context.
    Regional,
    /// One marine habitat column.
    Habitat,
}

/// Render a bounded ordinary or diagnostic Waterworld observation.
/// type-audit: bare-ok(flag: diagnostic), bare-ok(prose: return)
pub fn observe_waterworld(
    world: &WaterWorld,
    detail: WaterWorldDetail,
    diagnostic: bool,
) -> String {
    let scope = match detail {
        WaterWorldDetail::Planet => "planet",
        WaterWorldDetail::Regional => "region",
        WaterWorldDetail::Habitat => "habitat",
    };
    let mut output = format!(
        "Waterworld {scope}: {} substrate samples, {} current samples, {} aggregate stock samples",
        world.substrate.len(),
        world.propagation.accepted_count,
        world.stocks.len(),
    );
    if diagnostic {
        let vent_text = if world.vents.is_empty() {
            "no admitted vent source"
        } else {
            "admitted vent sources are present"
        };
        output.push_str(&format!(
            "; diagnostic fields include depth, pressure, light, temperature, salinity, chemistry, current; {vent_text}; values are derived, not certainty"
        ));
    }
    output
}
