//! Deterministic epidemic parameters and pure phase helpers.

use hornvale_kernel::{KindId, VertexMap};
use std::collections::BTreeMap;

/// Reservoir spillover probability per living community and epoch at unit fit.
/// type-audit: bare-ok(ratio)
/// plumb: universal(The Murrain spec section 4.4 spillover rate)
pub const SPILLOVER_RATE: f64 = 1.3e-3;
/// Traversable-hop radius of one epidemic wave.
/// type-audit: bare-ok(count)
/// plumb: universal(The Murrain spec section 4.4 wave radius)
pub const WAVE_RADIUS: u32 = 2;
/// Death share at which an occupation ends as plague.
/// type-audit: bare-ok(ratio)
/// plumb: universal(The Murrain spec section 4.4 plague threshold)
pub const PLAGUE_FRACTION: f64 = 0.30;

/// Composition-root inputs for one epidemic catalogue row.
/// type-audit: bare-ok(ratio: hosts), bare-ok(ratio: fit_by_era), bare-ok(ratio: attack_max), bare-ok(ratio: fatality), bare-ok(ratio: spillover_weight), bare-ok(flag: immunizing), bare-ok(count: ccs)
#[derive(Clone, Debug, PartialEq)]
pub struct EpidemicKind {
    /// Catalogue identity.
    pub kind: KindId,
    /// Relative susceptibility by host people.
    pub hosts: BTreeMap<KindId, f64>,
    /// One environmental-fit field per bake era.
    pub fit_by_era: Vec<VertexMap<f64>>,
    /// Maximum attack share.
    pub attack_max: f64,
    /// Fatality among attacked hosts.
    pub fatality: f64,
    /// Relative spillover weight.
    pub spillover_weight: f64,
    /// Whether prior survivors retain immunity.
    pub immunizing: bool,
    /// Critical connected host population, when persistence is defined.
    pub ccs: f64,
}

/// Fully resolved inputs for one community outbreak.
#[derive(Clone, Copy, Debug, PartialEq)]
pub(crate) struct OutbreakInput {
    pub(crate) kind: KindId,
    pub(crate) susceptible: f64,
    pub(crate) attack: f64,
    pub(crate) fatality: f64,
}

/// Susceptible share after an earlier immunising wave.
/// type-audit: bare-ok(count: year), bare-ok(count: last_struck), bare-ok(count: lifespan), bare-ok(ratio: return)
pub fn susceptible_share(year: f64, last_struck: Option<f64>, lifespan: f64) -> f64 {
    match last_struck {
        Some(last) => ((year - last) / lifespan).clamp(0.0, 1.0),
        None => 1.0,
    }
}
