//! The eight latent quality axes and their derivation from a [`Substrate`].
//!
//! Qualities are LATENT: nothing in the simulation reads them off a substance
//! directly. They are inferable — each one moves at least one manifest sign or
//! production outcome — but never observable. See [`crate::sign`].

use crate::{Substrate, clamp01};

/// One latent quality axis.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Quality {
    /// Survives fire unchanged.
    Fixity,
    /// Passes into air on heating.
    Volatility,
    /// Feeds fire.
    Combustibility,
    /// Yields to water.
    Solubility,
    /// Deforms without breaking.
    Malleability,
    /// Mass per bulk.
    Density,
    /// Attacks other matter.
    Causticity,
    /// Of living or once-living origin.
    Vitality,
}

impl Quality {
    /// Every axis, in declaration order. Iteration order is fixed so that any
    /// consumer folding over the axes is deterministic.
    pub const ALL: [Quality; 8] = [
        Quality::Fixity,
        Quality::Volatility,
        Quality::Combustibility,
        Quality::Solubility,
        Quality::Malleability,
        Quality::Density,
        Quality::Causticity,
        Quality::Vitality,
    ];
}

/// A substance's latent quality bundle. A material is a BUNDLE of qualities
/// exactly as a phoneme is a feature bundle; an atomic substance list would be
/// a lookup table, which is the catalogue-not-a-language failure this design
/// exists to avoid.
///
/// `fixity` and `volatility` are deliberately NOT forced complements — a
/// substance may be low in both (it decomposes rather than surviving or flying
/// off), and that region is where the interesting productions live.
/// type-audit: bare-ok(ratio: fixity), bare-ok(ratio: volatility), bare-ok(ratio: combustibility), bare-ok(ratio: solubility), bare-ok(ratio: malleability), bare-ok(ratio: density), bare-ok(ratio: causticity), bare-ok(ratio: vitality)
#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub struct QualityVector {
    /// Survives fire unchanged.
    pub fixity: f64,
    /// Passes into air on heating.
    pub volatility: f64,
    /// Feeds fire.
    pub combustibility: f64,
    /// Yields to water.
    pub solubility: f64,
    /// Deforms without breaking.
    pub malleability: f64,
    /// Mass per bulk.
    pub density: f64,
    /// Attacks other matter.
    pub causticity: f64,
    /// Of living or once-living origin.
    pub vitality: f64,
}

impl QualityVector {
    /// Read one axis by name.
    /// type-audit: bare-ok(ratio: return)
    pub fn get(&self, q: Quality) -> f64 {
        match q {
            Quality::Fixity => self.fixity,
            Quality::Volatility => self.volatility,
            Quality::Combustibility => self.combustibility,
            Quality::Solubility => self.solubility,
            Quality::Malleability => self.malleability,
            Quality::Density => self.density,
            Quality::Causticity => self.causticity,
            Quality::Vitality => self.vitality,
        }
    }

    /// Write one axis by name, clamped to [0,1].
    /// type-audit: bare-ok(ratio: v)
    pub fn set(&mut self, q: Quality, v: f64) {
        let v = clamp01(v);
        match q {
            Quality::Fixity => self.fixity = v,
            Quality::Volatility => self.volatility = v,
            Quality::Combustibility => self.combustibility = v,
            Quality::Solubility => self.solubility = v,
            Quality::Malleability => self.malleability = v,
            Quality::Density => self.density = v,
            Quality::Causticity => self.causticity = v,
            Quality::Vitality => self.vitality = v,
        }
    }
}

/// Derive a substance's latent qualities from its substrate.
///
/// Linear forms with clamping — deliberately simple and explainable, because
/// this is a tuning surface with no data behind it and the accuracy metric of
/// a later campaign is what will eventually judge the coefficients.
pub fn qualities_of(s: &Substrate) -> QualityVector {
    QualityVector {
        fixity: clamp01(0.55 * s.refractory + 0.35 * s.metallic - 0.30 * s.organic + 0.20),
        volatility: clamp01(0.60 * s.organic + 0.30 * s.saline - 0.40 * s.refractory + 0.15),
        combustibility: clamp01(0.75 * s.organic - 0.35 * s.metallic + 0.10),
        solubility: clamp01(0.80 * s.saline - 0.45 * s.metallic - 0.20 * s.refractory + 0.25),
        malleability: clamp01(0.80 * s.metallic - 0.30 * s.refractory + 0.05),
        density: clamp01(0.70 * s.metallic + 0.15 * s.refractory - 0.35 * s.organic + 0.25),
        // Impurity is caustic: this is where The Lode's drawn ore `grade`
        // reaches the quality layer, and therefore where the provenance
        // confound gets its physical mechanism.
        causticity: clamp01(0.55 * s.saline + 0.25 * (1.0 - s.purity) + 0.10),
        vitality: clamp01(s.organic),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Substrate;

    /// Every axis of the derived vector stays inside [0,1] for the extreme
    /// corners of the substrate space — the invariant every later stage
    /// assumes.
    #[test]
    fn qualities_stay_in_range_at_the_corners() {
        for metallic in [0.0, 1.0] {
            for organic in [0.0, 1.0] {
                for saline in [0.0, 1.0] {
                    for refractory in [0.0, 1.0] {
                        for purity in [0.0, 1.0] {
                            let s = Substrate {
                                metallic,
                                organic,
                                saline,
                                refractory,
                                purity,
                            };
                            let q = qualities_of(&s);
                            for axis in Quality::ALL {
                                let v = q.get(axis);
                                assert!(
                                    (0.0..=1.0).contains(&v),
                                    "{axis:?} out of range at {s:?}: {v}"
                                );
                            }
                        }
                    }
                }
            }
        }
    }

    /// Impurity raises causticity: the hook by which The Lode's already-drawn
    /// ore `grade` reaches the quality layer. This is the mechanism the
    /// provenance confound runs on — a doctrine formed on rich ore meets poor
    /// ore in the next valley and fails.
    #[test]
    fn impurity_raises_causticity() {
        let rich = Substrate {
            metallic: 0.9,
            organic: 0.0,
            saline: 0.0,
            refractory: 0.7,
            purity: 0.9,
        };
        let poor = Substrate {
            purity: 0.1,
            ..rich
        };
        assert!(
            qualities_of(&poor).causticity > qualities_of(&rich).causticity,
            "poor ore must be more caustic than rich ore"
        );
    }

    /// Vitality is organic origin, carried through unchanged.
    #[test]
    fn vitality_tracks_organic_origin() {
        let living = Substrate {
            metallic: 0.0,
            organic: 1.0,
            saline: 0.0,
            refractory: 0.0,
            purity: 1.0,
        };
        let stone = Substrate {
            organic: 0.0,
            ..living
        };
        assert_eq!(qualities_of(&living).vitality, 1.0);
        assert_eq!(qualities_of(&stone).vitality, 0.0);
    }
}
