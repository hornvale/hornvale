//! Sky genesis assembly: world seed in, complete star system out.

use crate::anchor::{Anchor, generate_anchor};
use crate::moons::{Moon, generate_moons};
use crate::neighborhood::{Neighbor, generate_neighbors};
use crate::pins::{GenesisError, SkyPins};
use crate::star::{Star, generate_star};
use crate::streams;
use hornvale_kernel::Seed;

/// A complete generated star system.
#[derive(Debug, Clone, PartialEq)]
pub struct StarSystem {
    /// The main-sequence host star.
    pub star: Star,
    /// The habitable anchor world.
    pub anchor: Anchor,
    /// Moons, nearest first.
    pub moons: Vec<Moon>,
    /// Notable neighbor stars, brightest first.
    pub neighbors: Vec<Neighbor>,
}

/// Generate the sky for a world: anchor-first, pins conditioned on,
/// loud failure on unsatisfiable pins. Takes the WORLD seed and derives
/// the astronomy domain seed internally.
pub fn generate(world_seed: Seed, pins: &SkyPins) -> Result<StarSystem, GenesisError> {
    let astronomy_seed = world_seed.derive(streams::ROOT);
    let star = generate_star(astronomy_seed);
    let anchor = generate_anchor(astronomy_seed, &star, pins)?;
    let moons = generate_moons(astronomy_seed, &star, &anchor, pins)?;
    let neighbors = generate_neighbors(astronomy_seed, pins);
    Ok(StarSystem {
        star,
        anchor,
        moons,
        neighbors,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn generate_assembles_a_complete_system() {
        let system = generate(Seed(42), &SkyPins::default()).unwrap();
        assert!(system.star.luminosity.get() > 0.0);
        assert!(system.anchor.year.get() > 0.0);
        assert!(system.moons.len() <= 3);
        assert!(!system.neighbors.is_empty());
    }

    #[test]
    fn generate_is_deterministic() {
        let a = generate(Seed(42), &SkyPins::default()).unwrap();
        let b = generate(Seed(42), &SkyPins::default()).unwrap();
        assert_eq!(a, b);
    }
}
