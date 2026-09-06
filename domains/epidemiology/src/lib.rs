//! Pure epidemiology rules over population numbers and occupied graph sites.
//!
//! This crate deliberately has no window or Lot dependency.  Worldgen and
//! other consumers provide substrate-derived numbers and graph edges; this
//! crate only applies the disease rules to those plain values.
#![warn(missing_docs)]

use hornvale_kernel::{ConceptRegistry, RegistryError};
use std::collections::BTreeSet;

/// Dated event naming the pathogen that struck an occupation.
/// type-audit: bare-ok(identifier-text)
pub const STRUCK_BY: &str = "struck-by";
/// Dated event recording deaths in the corresponding outbreak.
/// type-audit: bare-ok(identifier-text)
pub const OUTBREAK_DEATHS: &str = "outbreak-deaths";

/// Register epidemiology's paired outbreak-event predicates.
pub fn register_concepts(registry: &mut ConceptRegistry) -> Result<(), RegistryError> {
    registry.register_predicate(STRUCK_BY, false, "the pathogen in a dated outbreak event")?;
    registry.register_predicate(
        OUTBREAK_DEATHS,
        false,
        "the deaths in a dated outbreak event",
    )?;
    Ok(())
}

/// Epidemiology as a registrable domain unit.
pub struct Epidemiology;

impl hornvale_kernel::Domain for Epidemiology {
    fn crate_name(&self) -> &'static str {
        env!("CARGO_PKG_NAME")
    }

    fn register_concepts(&self, registry: &mut ConceptRegistry) -> Result<(), RegistryError> {
        register_concepts(registry)
    }
}

/// The trough constant calibrated from the measles-shaped anchor.
/// type-audit: bare-ok(ratio)
/// plumb: universal(the fixed trough calibration shared by every pathogen row; it is the measles anchor's derived constant, not a world value)
pub const CCS_TROUGH: f64 = 250_000.0 * (1.0 / 30.0) * (1.0 - 1.0 / 15.0) * (8.0 / 365.25);

/// Critical host population for an acute pathogen to persist.
///
/// The anchor is a measles-shaped pathogen at 250,000 hosts.  The formula is
/// intentionally over plain numbers so callers can supply a substrate view.
/// type-audit: bare-ok(ratio: r0), bare-ok(count: infectious_years), bare-ok(ratio: births_per_person_year), bare-ok(ratio: return)
pub fn critical_community_size(r0: f64, infectious_years: f64, births_per_person_year: f64) -> f64 {
    assert!(r0 > 1.0, "R0 must exceed one");
    assert!(infectious_years > 0.0, "infectious period must be positive");
    assert!(births_per_person_year > 0.0, "birth rate must be positive");
    CCS_TROUGH / (births_per_person_year * (1.0 - 1.0 / r0) * infectious_years)
}

/// Whether a connected host population reaches its critical size.
/// type-audit: bare-ok(count: population), bare-ok(count: ccs), bare-ok(flag: return)
pub fn persists(population: f64, ccs: f64) -> bool {
    population > ccs
}

/// Return occupied vertices within `radius` traversable hops of `origin`.
///
/// The input is `(vertex, occupied neighbours)` and only vertices present in
/// the input may be reached.  The result is ascending by vertex id, making it
/// independent of edge-storage order.
/// type-audit: bare-ok(count: origin), bare-ok(count: radius), bare-ok(count: occupied_neighbors), bare-ok(count: return)
pub fn wave_reach(origin: u32, occupied_neighbors: &[(u32, &[u32])], radius: u32) -> Vec<u32> {
    let occupied: BTreeSet<u32> = occupied_neighbors.iter().map(|(v, _)| *v).collect();
    if !occupied.contains(&origin) {
        return Vec::new();
    }

    let mut reached = BTreeSet::from([origin]);
    let mut frontier = vec![origin];
    for _ in 0..radius {
        let mut next = Vec::new();
        for vertex in frontier {
            let Some((_, neighbours)) = occupied_neighbors.iter().find(|(v, _)| *v == vertex)
            else {
                continue;
            };
            for &neighbour in *neighbours {
                if occupied.contains(&neighbour) && reached.insert(neighbour) {
                    next.push(neighbour);
                }
            }
        }
        frontier = next;
        if frontier.is_empty() {
            break;
        }
    }
    reached.into_iter().collect()
}

/// Arithmetic result of one outbreak at one population site.
/// type-audit: bare-ok(count: deaths), bare-ok(count: population_after), bare-ok(flag: ends_as_plague)
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct OutbreakOutcome {
    /// Number of deaths caused by the outbreak.
    /// type-audit: bare-ok(count)
    pub deaths: f64,
    /// Population remaining after the outbreak.
    /// type-audit: bare-ok(count)
    pub population_after: f64,
    /// Whether the death share reaches the plague-ending threshold.
    /// type-audit: bare-ok(flag)
    pub ends_as_plague: bool,
}

/// Apply attack and fatality to a susceptible population share.
/// type-audit: bare-ok(count: population), bare-ok(ratio: susceptible), bare-ok(ratio: attack), bare-ok(ratio: fatality), bare-ok(ratio: plague_fraction)
pub fn outbreak(
    population: f64,
    susceptible: f64,
    attack: f64,
    fatality: f64,
    plague_fraction: f64,
) -> OutbreakOutcome {
    assert!(population >= 0.0, "population must not be negative");
    assert!(
        (0.0..=1.0).contains(&susceptible),
        "susceptibility must be a share"
    );
    assert!((0.0..=1.0).contains(&attack), "attack must be a share");
    assert!((0.0..=1.0).contains(&fatality), "fatality must be a share");
    assert!(
        (0.0..=1.0).contains(&plague_fraction),
        "plague threshold must be a share"
    );
    let death_share = susceptible * attack * fatality;
    let deaths = population * death_share;
    OutbreakOutcome {
        deaths,
        population_after: population - deaths,
        ends_as_plague: death_share >= plague_fraction,
    }
}
