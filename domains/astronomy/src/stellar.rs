//! Stellar-system topology and deterministic binary parameters.

use crate::pins::{GenesisError, SkyPins};
use crate::star::{Star, luminosity_at, star_with_mass_and_age};
use crate::streams;
use crate::system::StarSystem;
use crate::units::{Au, HabitableZone, SolarLuminosities, SolarMasses, StdDays, StdInstant};
use hornvale_kernel::Seed;

/// The supported stellar roots.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StellarTopology {
    /// One host star.
    Single,
    /// A wide binary whose anchor follows a circumprimary orbit.
    WideBinary,
    /// A close binary whose anchor follows a circumbinary orbit.
    CloseBinary,
}

/// The circular two-body orbit of a binary pair.
/// type-audit: bare-ok(ratio: phase)
#[derive(Debug, Clone, PartialEq)]
pub struct BinaryOrbit {
    /// Separation between the two stars, in AU.
    pub semi_major_axis: Au,
    /// Two-body orbital period in standard days.
    pub period: StdDays,
    /// Orbital phase at genesis, in turns in `[0, 1)`.
    pub phase: f64,
}

/// The secondary star and its orbit relative to the primary.
#[derive(Debug, Clone, PartialEq)]
pub struct CompanionStar {
    /// The secondary main-sequence star.
    pub star: Star,
    /// The binary pair's bounded two-body orbit.
    pub orbit: BinaryOrbit,
}

/// The stellar root and the anchor-admission metadata derived from it.
#[derive(Debug, Clone, PartialEq)]
pub struct StellarConfiguration {
    /// The root topology.
    pub topology: StellarTopology,
    /// The secondary star and binary orbit, absent for a single star.
    pub companion: Option<CompanionStar>,
    /// Radiative habitable zone used to place the anchor.
    pub anchor_habitable_zone: HabitableZone,
    /// Outermost admitted circumprimary orbit, present only for wide binaries.
    pub circumprimary_outer_limit: Option<Au>,
    /// Innermost admitted circumbinary orbit, present only for close binaries.
    pub circumbinary_inner_limit: Option<Au>,
    /// Sum of the stars' genesis luminosities.
    pub combined_luminosity: SolarLuminosities,
}

// A conservative circular-binary envelope for the first bounded model:
// circumprimary worlds remain inside one fifth of the stellar separation.
/// plumb: pending(wave-1)
const CIRCUMPRIMARY_STABILITY_FACTOR: f64 = 0.20;
// Circumbinary worlds remain outside three stellar separations.
/// plumb: pending(wave-1)
const CIRCUMBINARY_STABILITY_FACTOR: f64 = 3.0;

fn zone_for_luminosity(luminosity: SolarLuminosities) -> HabitableZone {
    let root = luminosity.get().sqrt();
    HabitableZone::new(Au(0.95 * root), Au(1.37 * root))
        .expect("0.95√L < 1.37√L for every positive luminosity")
}

impl StellarConfiguration {
    /// The compatibility configuration for one primary star.
    pub fn single(primary: &Star) -> StellarConfiguration {
        StellarConfiguration {
            topology: StellarTopology::Single,
            companion: None,
            anchor_habitable_zone: primary.habitable_zone,
            circumprimary_outer_limit: None,
            circumbinary_inner_limit: None,
            combined_luminosity: primary.luminosity,
        }
    }

    pub(crate) fn gravity_mass(&self, primary: &Star) -> SolarMasses {
        match (&self.topology, &self.companion) {
            (StellarTopology::CloseBinary, Some(companion)) => {
                SolarMasses(primary.mass.get() + companion.star.mass.get())
            }
            _ => primary.mass,
        }
    }
}

/// Generate a deterministic stellar root around an already-generated primary.
///
/// The topology draw is always consumed before a pin is applied. Binary mass,
/// separation, and phase then come from one appended binary-parameter stream;
/// neither path can move the existing primary-star draw.
pub fn generate_stellar(
    astronomy_seed: Seed,
    primary: &Star,
    pins: &SkyPins,
) -> Result<StellarConfiguration, GenesisError> {
    let topology_roll = astronomy_seed
        .derive(streams::STELLAR_TOPOLOGY)
        .stream()
        .next_f64();
    let drawn_topology = if topology_roll < 0.90 {
        StellarTopology::Single
    } else if topology_roll < 0.97 {
        StellarTopology::WideBinary
    } else {
        StellarTopology::CloseBinary
    };
    let topology = pins.topology.unwrap_or(drawn_topology);

    if topology == StellarTopology::Single {
        return Ok(StellarConfiguration::single(primary));
    }

    let mut stream = astronomy_seed.derive(streams::BINARY_PARAMETERS).stream();
    let companion_mass = SolarMasses(0.6 + stream.next_f64() * (primary.mass.get() - 0.6));
    // Bound stars are coeval. Keeping the secondary no heavier than the
    // primary also keeps this age inside its longer main-sequence lifetime.
    let companion_star = star_with_mass_and_age(companion_mass, primary.age);
    let combined_luminosity =
        SolarLuminosities(primary.luminosity.get() + companion_star.luminosity.get());
    let anchor_habitable_zone = match topology {
        StellarTopology::WideBinary => primary.habitable_zone,
        StellarTopology::CloseBinary => zone_for_luminosity(combined_luminosity),
        StellarTopology::Single => unreachable!("single returned above"),
    };

    // Put the stability boundary strictly inside the applicable habitable
    // zone. The anchor draw is then admitted on the appropriate side, while
    // a conflicting pinned year can still be refused explicitly.
    let boundary = Au(anchor_habitable_zone.inner().get()
        + (0.25 + 0.50 * stream.next_f64())
            * (anchor_habitable_zone.outer().get() - anchor_habitable_zone.inner().get()));
    let (semi_major_axis, circumprimary_outer_limit, circumbinary_inner_limit) = match topology {
        StellarTopology::WideBinary => (
            Au(boundary.get() / CIRCUMPRIMARY_STABILITY_FACTOR),
            Some(boundary),
            None,
        ),
        StellarTopology::CloseBinary => (
            Au(boundary.get() / CIRCUMBINARY_STABILITY_FACTOR),
            None,
            Some(boundary),
        ),
        StellarTopology::Single => unreachable!("single returned above"),
    };
    let total_mass = primary.mass.get() + companion_star.mass.get();
    let period = StdDays(365.25 * (semi_major_axis.get().powi(3) / total_mass).sqrt());
    let phase = stream.next_f64();

    Ok(StellarConfiguration {
        topology,
        companion: Some(CompanionStar {
            star: companion_star,
            orbit: BinaryOrbit {
                semi_major_axis,
                period,
                phase,
            },
        }),
        anchor_habitable_zone,
        circumprimary_outer_limit,
        circumbinary_inner_limit,
        combined_luminosity,
    })
}

/// Gravitational mass governing the anchor orbit: primary-only for a single
/// or wide circumprimary system, and total stellar mass for a close
/// circumbinary system.
pub fn stellar_gravity_mass(system: &StarSystem) -> SolarMasses {
    system.stellar.gravity_mass(&system.star)
}

/// Total luminosity of every star in the stellar root at `instant`.
///
/// This is stellar metadata, not an inverse-square flux at the anchor. The
/// wide companion's climate contribution remains negligible in this first
/// approximation even though its own luminosity is represented here.
pub fn stellar_luminosity_at(system: &StarSystem, instant: StdInstant) -> SolarLuminosities {
    let primary = luminosity_at(&system.star, instant).get();
    let companion = system
        .stellar
        .companion
        .as_ref()
        .map_or(0.0, |star| luminosity_at(&star.star, instant).get());
    SolarLuminosities(primary + companion)
}
