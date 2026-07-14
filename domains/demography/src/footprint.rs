//! Kleiber home-range scaling: a cell's fractional occupancy per individual
//! (the "grain" of a species — super-linear with body mass). A small species
//! (mice) might occupy <0.01 cells/individual (many pack per cell); a large
//! species (deer) ~1 cell/individual; a large predator or megafauna many cells
//! per individual. Allometry follows [Kleiber's law](https://en.wikipedia.org/wiki/Kleiber's_law):
//! home range scales super-linearly with body mass.

use hornvale_kernel::Mass;
use hornvale_kernel::math;

// CALIBRATED (none yet, placeholder authored): a super-linear Kleiber-like
// home-range exponent (2/3 power in metabolic scaling is sub-linear; home
// range biology suggests 1.0–1.5 exponent depending on ecology). Chosen 1.25
// as a middle ground: measurably super-linear (100 kg ratio → 314× range at
// this exponent), leaving room for variation with habitat type (plains vs
// forest, pursuit vs ambush).
const EXPONENT: f64 = 1.25;

// CALIBRATED (none yet, placeholder authored): scale constant chosen so a
// mid-body (e.g. 40 kg, goblin-sized) lands near 1 cell/individual (the
// transition between high-density small species and low-density large ones).
// Formula: 1 / (40^EXPONENT) ≈ 0.01.
const A: f64 = 0.01;

/// Home range in cells per individual (Kleiber super-linear allometry in mass).
/// Returns the fractional occupancy of a single individual of the given mass:
/// values << 1.0 mean many pack per cell; values >> 1.0 mean one individual
/// spans many cells.
///
/// type-audit: bare-ok(ratio: return)
pub fn home_range(mass: Mass) -> f64 {
    let mass_kg = mass.kilograms();
    A * math::powf(mass_kg, EXPONENT)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn home_range_is_monotone_and_superlinear_in_mass() {
        let small = home_range(Mass::new(1.0).unwrap());
        let goblin = home_range(Mass::new(40.0).unwrap());
        let dragon = home_range(Mass::new(4000.0).unwrap());
        assert!(
            small < goblin && goblin < dragon,
            "bigger body → larger range"
        );
        // super-linear: 100× mass → >100× range
        assert!(dragon / goblin > 100.0, "Kleiber super-linearity");
    }
}
