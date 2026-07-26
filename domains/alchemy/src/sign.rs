//! The five manifest sign channels — everything an observer can actually
//! perceive of a substance.
//!
//! Each sign is a PARTIAL and partly misleading function of the latent
//! [`crate::quality::QualityVector`]. The faithfulness gradient is deliberate
//! and is the design's whole point of contact with the historical doctrine of
//! signatures: `heft` is near-faithful, `hue` is weak and misleading. A
//! practitioner reasoning from colour will be wrong in a specific, derivable,
//! reproducible way.

use crate::clamp01;
use crate::quality::QualityVector;

/// One manifest sign channel.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Sign {
    /// Perceived weight. Near-faithful to density.
    Heft,
    /// Perceived texture. A good read of fixity and malleability.
    Grain,
    /// Perceived shine. A moderate read.
    Lustre,
    /// Perceived smell. Weak — conflates volatility with vitality.
    Odour,
    /// Perceived colour. Weak and misleading — conflates causticity with
    /// vitality, and is the mechanized doctrine of signatures.
    Hue,
}

/// What an observer gets. Every field is a dimensionless ratio in [0,1].
/// type-audit: bare-ok(ratio: heft), bare-ok(ratio: grain), bare-ok(ratio: lustre), bare-ok(ratio: odour), bare-ok(ratio: hue)
#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub struct SignVector {
    /// Perceived weight.
    pub heft: f64,
    /// Perceived texture.
    pub grain: f64,
    /// Perceived shine.
    pub lustre: f64,
    /// Perceived smell.
    pub odour: f64,
    /// Perceived colour.
    pub hue: f64,
}

/// Project latent qualities into what can be perceived.
///
/// The coefficients encode the faithfulness gradient documented on [`Sign`].
/// `hue`'s two coefficients are close together on purpose: it is the channel
/// that must FAIL to distinguish substances a practitioner cares about.
pub fn signs_of(q: &QualityVector) -> SignVector {
    SignVector {
        heft: clamp01(0.95 * q.density + 0.05),
        grain: clamp01(0.55 * q.fixity + 0.40 * q.malleability),
        lustre: clamp01(0.60 * q.malleability + 0.30 * q.fixity),
        odour: clamp01(0.50 * q.volatility + 0.45 * q.vitality),
        hue: clamp01(0.50 * q.causticity + 0.45 * q.vitality),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::quality::QualityVector;

    /// Heft is a near-faithful read of density: a practitioner who reasons
    /// from weight is reasoning well.
    #[test]
    fn heft_tracks_density_faithfully() {
        let heavy = QualityVector {
            density: 0.9,
            ..QualityVector::default()
        };
        let light = QualityVector {
            density: 0.1,
            ..QualityVector::default()
        };
        assert!(signs_of(&heavy).heft > signs_of(&light).heft);
    }

    /// THE CONFOUND, MECHANIZED. `hue` conflates causticity with vitality, so
    /// a violently caustic mineral and an inert living thing present the SAME
    /// colour. A practitioner reasoning "the root is red, so it treats blood"
    /// is therefore wrong for a derivable reason -- and nobody authored a
    /// superstition to make it happen.
    ///
    /// If this test ever fails because the two hues diverge, the doctrine of
    /// signatures has become sound in-world and the program has lost its
    /// subject matter. Do not "fix" it by making hue faithful.
    #[test]
    fn hue_collides_across_utterly_different_substances() {
        let caustic_mineral = QualityVector {
            causticity: 0.9,
            vitality: 0.0,
            ..QualityVector::default()
        };
        let inert_living = QualityVector {
            causticity: 0.0,
            vitality: 1.0,
            ..QualityVector::default()
        };

        let a = signs_of(&caustic_mineral);
        let b = signs_of(&inert_living);

        assert_eq!(a.hue, b.hue, "hue must not distinguish these two");
        assert_ne!(
            caustic_mineral.vitality, inert_living.vitality,
            "...while the substances differ maximally in what matters"
        );
    }

    /// The projection is total: every quality vector yields signs in range.
    #[test]
    fn signs_stay_in_range() {
        for v in [0.0, 0.5, 1.0] {
            let q = QualityVector {
                fixity: v,
                volatility: v,
                combustibility: v,
                solubility: v,
                malleability: v,
                density: v,
                causticity: v,
                vitality: v,
            };
            let s = signs_of(&q);
            for channel in [s.heft, s.grain, s.lustre, s.odour, s.hue] {
                assert!(
                    (0.0..=1.0).contains(&channel),
                    "sign out of range: {channel}"
                );
            }
        }
    }
}
