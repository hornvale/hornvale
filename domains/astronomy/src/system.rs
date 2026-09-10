//! Sky genesis assembly: world seed in, complete star system out.

use crate::anchor::{Anchor, generate_anchor_for_stellar};
use crate::moons::{Moon, generate_moons};
use crate::neighborhood::{Neighbor, generate_neighbors};
use crate::pins::{GenesisError, SkyPins};
use crate::star::{Star, generate_star};
use crate::stellar::{StellarConfiguration, generate_stellar};
use crate::streams;
use crate::wanderers::{Wanderer, generate_wanderers_with_mass};
use hornvale_kernel::Seed;

/// A complete generated star system.
#[derive(Debug, Clone, PartialEq)]
pub struct StarSystem {
    /// The main-sequence host star.
    pub star: Star,
    /// Stellar topology and binary metadata. `star` remains the primary view.
    pub stellar: StellarConfiguration,
    /// The habitable anchor world.
    pub anchor: Anchor,
    /// Moons, nearest first.
    pub moons: Vec<Moon>,
    /// Notable neighbor stars, brightest first.
    pub neighbors: Vec<Neighbor>,
    /// Deep-time orbital forcing (Milankovitch triad).
    pub forcing: crate::forcing::OrbitalForcing,
    /// Wandering sibling planets, innermost first (observational: no physical effect on the anchor — declared approximation).
    pub wanderers: Vec<Wanderer>,
}

pub use hornvale_kernel::genesis::GenesisOutcome;

/// Generate the sky for a world: anchor-first, pins conditioned on,
/// loud failure on unsatisfiable pins. Takes the WORLD seed and derives
/// the astronomy domain seed internally. Returns the system and any notes
/// about degradations that occurred.
pub fn generate(
    world_seed: Seed,
    pins: &SkyPins,
) -> Result<GenesisOutcome<StarSystem>, GenesisError> {
    let astronomy_seed = world_seed.derive(streams::ROOT);
    let star = generate_star(astronomy_seed);
    let stellar = generate_stellar(astronomy_seed, &star, pins)?;
    let anchor = generate_anchor_for_stellar(astronomy_seed, &star, &stellar, pins)?;
    let (moons, notes) = generate_moons(astronomy_seed, &star, &anchor, pins)?;
    let neighbors = generate_neighbors(astronomy_seed, pins);
    let forcing = crate::forcing::generate_forcing(astronomy_seed, &anchor, &moons, pins);
    let wanderers =
        generate_wanderers_with_mass(astronomy_seed, stellar.gravity_mass(&star), &anchor, pins);
    Ok(GenesisOutcome {
        value: StarSystem {
            star,
            stellar,
            anchor,
            moons,
            neighbors,
            forcing,
            wanderers,
        },
        notes,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn generate_assembles_a_complete_system() {
        let outcome = generate(Seed(42), &SkyPins::default()).unwrap();
        let system = &outcome.value;
        assert!(system.star.luminosity.get() > 0.0);
        assert!(system.anchor.year.get() > 0.0);
        assert!(system.moons.len() <= 3);
        assert!(!system.neighbors.is_empty());
        assert!(system.wanderers.len() <= 4);
    }

    #[test]
    fn generate_is_deterministic() {
        let a = generate(Seed(42), &SkyPins::default()).unwrap();
        let b = generate(Seed(42), &SkyPins::default()).unwrap();
        assert_eq!(a, b);
    }
}
