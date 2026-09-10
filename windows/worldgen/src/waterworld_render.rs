//! Pure Waterworld observation detail.

use crate::waterworld::{VentState, WaterWorkCounters, WaterWorld, WaterWorldSnapshot};

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

/// A rendered observation and the bounded work used to prepare it.
/// type-audit: bare-ok(prose: text)
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct WaterWorldObservation {
    /// Human-facing observation text.
    pub text: String,
    /// Observation-only work; snapshot derivation counters remain untouched.
    pub counters: WaterWorkCounters,
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

/// Observe one already-derived temporal snapshot without rebuilding or mutating it.
///
/// The ordinary view reports present consequences. The diagnostic view adds
/// explicitly derived source information; it never upgrades inference into a
/// direct observation.
/// type-audit: bare-ok(flag: diagnostic)
pub fn observe_waterworld_snapshot(
    world: &WaterWorld,
    snapshot: &WaterWorldSnapshot,
    detail: WaterWorldDetail,
    diagnostic: bool,
) -> WaterWorldObservation {
    assert_eq!(
        world.substrate.len(),
        snapshot.fields.len(),
        "Waterworld substrate and snapshot fields must remain aligned"
    );
    assert_eq!(
        world.substrate.len(),
        snapshot.stocks.len(),
        "Waterworld substrate and snapshot stocks must remain aligned"
    );
    assert_eq!(
        world.vents.len(),
        snapshot.vent_states.len(),
        "Waterworld sources and snapshot phases must remain aligned"
    );
    assert_eq!(
        world.vents.len(),
        snapshot.vent_phase_positions.len(),
        "Waterworld sources and snapshot phase positions must remain aligned"
    );

    let scope = scope(detail);
    let mut seabed = 0;
    let mut bloom = 0;
    let mut nutrients = 0;
    let mut kelp_reef = 0;
    let mut transported = 0;
    let mut local = 0;
    let mut zero_baseline = 0;
    for ((substrate, fields), stocks) in world
        .substrate
        .iter()
        .zip(&snapshot.fields)
        .zip(&snapshot.stocks)
    {
        seabed += usize::from(substrate.is_seabed);
        bloom += usize::from(stocks.chemosynthetic_bloom > 0.0);
        nutrients += usize::from(stocks.nutrients > 0.0);
        kelp_reef += usize::from(stocks.kelp_reef > 0.0);
        transported += usize::from(stocks.transported_influence > 0.0);
        local += usize::from(stocks.local_source_influence > 0.0);
        zero_baseline += usize::from(
            fields.chemistry == 0.0
                && stocks.local_source_influence == 0.0
                && stocks.transported_influence == 0.0,
        );
    }
    let mut counters = WaterWorkCounters {
        observation: world.substrate.len(),
        ..WaterWorkCounters::default()
    };
    let mut text = format!(
        "Waterworld {scope}; marine substrate: {} samples ({} seabed); present stocks: bloom in {}, nutrients in {}, reef/kelp suitability in {}; vent consequence: {} locally influenced samples; current transport: {} influenced samples",
        world.substrate.len(),
        seabed,
        bloom,
        nutrients,
        kelp_reef,
        local,
        transported
    );

    if diagnostic {
        let mut states = [0_usize; 5];
        for state in &snapshot.vent_states {
            counters.observation += 1;
            states[state_index(*state)] += 1;
        }
        text.push_str(&format!(
            "; source phase (derived, not directly observed): absent {}, nascent {}, active {}, weakening {}, failed {}; provenance: stable vent source identities from admitted seabed context; local/transported split: {} local samples / {} transported samples; absent contribution (source remains admitted): {}; failed contribution (source and seabed remain present): {}; zero ambient baseline (measured value is zero): {}; phase and source are an inferred cause; uncertain at observation scale",
            states[0],
            states[1],
            states[2],
            states[3],
            states[4],
            local,
            transported,
            states[0],
            states[4],
            zero_baseline,
        ));
    }

    WaterWorldObservation { text, counters }
}

fn scope(detail: WaterWorldDetail) -> &'static str {
    match detail {
        WaterWorldDetail::Planet => "planet",
        WaterWorldDetail::Regional => "region",
        WaterWorldDetail::Habitat => "habitat",
    }
}

fn state_index(state: VentState) -> usize {
    match state {
        VentState::Absent => 0,
        VentState::Nascent => 1,
        VentState::Active => 2,
        VentState::Weakening => 3,
        VentState::Failed => 4,
    }
}
