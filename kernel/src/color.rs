//! Spectral colour: the substrate for colour as a three-way product of
//! illuminant, reflectance and observer (spec "The Pigment").
//!
//! Colour is not a property of an object. A material has a *reflectance* —
//! the fraction of light it returns per wavelength, identical in a cave and
//! at noon. Light has a spectrum. An eye has sensitivity curves and
//! collapses the arriving mixture to one number per channel. Colour exists
//! only where all three meet, which is why every observer variation
//! (species vision, colour blindness, a screen reader taking none of it) is
//! the same operation with a different observer.
//!
//! **Determinism.** The hot path is `Σ r[b] · i[b] · s[b]` — multiplication
//! and addition over fixed-size arrays, which IEEE 754 requires to be
//! exact (decision 0041). No `math.rs` call appears here. Use `a * b + c`
//! and never `mul_add`: both are exact but they round differently from
//! each other.

use crate::units::UnitError;

/// Number of sampled wavelength bands. **This is a contract**: widening it
/// rewrites every authored reflectance in the workspace, which is why the
/// grid already reaches into the near-ultraviolet rather than stopping at
/// the human visible range.
/// type-audit: bare-ok(count)
pub const BANDS: usize = 10;

/// Band **centre** wavelengths in nanometres. The grid is ten uniform 40 nm
/// bands whose *edges* span 340–740 nm, so the centres run 360–720: band 0
/// covers 340–380 and is centred at 360, band 9 covers 700–740 and is
/// centred at 720. Anything integrating over a band (Planck sampling, a
/// sensitivity curve) wants the edges; anything sampling a point wants
/// these.
///
/// Uniform rather than weighted toward human cone peaks: a human-weighted
/// grid would rebuild, in the substrate, exactly the anthropocentrism this
/// model exists to remove.
/// type-audit: bare-ok(ratio)
pub const BAND_CENTERS_NM: [f64; BANDS] = [
    360.0, 400.0, 440.0, 480.0, 520.0, 560.0, 600.0, 640.0, 680.0, 720.0,
];

/// A quantity sampled on the band grid. Unconstrained in magnitude — a
/// radiance may exceed 1 where a reflectance may not.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Spectrum([f64; BANDS]);

impl Spectrum {
    /// Validating constructor: every band must be finite.
    /// type-audit: bare-ok(constructor-edge: bands)
    pub fn new(bands: [f64; BANDS]) -> Result<Self, UnitError> {
        for value in bands {
            if !value.is_finite() {
                return Err(UnitError {
                    unit: "spectrum",
                    value,
                    reason: "every band must be finite",
                });
            }
        }
        Ok(Self(bands))
    }

    /// The raw per-band values.
    /// type-audit: bare-ok(constructor-edge: return)
    pub fn get(&self) -> &[f64; BANDS] {
        &self.0
    }
}

/// The fraction of arriving light a material returns, per band. A property
/// of the stuff: the same in a cave and at noon.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Reflectance(Spectrum);

impl Reflectance {
    /// Validating constructor: every band is finite and within `[0, 1]`.
    /// A band above 1 would return more light than arrived.
    /// type-audit: bare-ok(constructor-edge: bands)
    pub fn new(bands: [f64; BANDS]) -> Result<Self, UnitError> {
        for value in bands {
            if !value.is_finite() || !(0.0..=1.0).contains(&value) {
                return Err(UnitError {
                    unit: "reflectance",
                    value,
                    reason: "every band must be finite and within [0, 1]",
                });
            }
        }
        Ok(Self(Spectrum(bands)))
    }

    /// The raw per-band fractions.
    /// type-audit: bare-ok(constructor-edge: return)
    pub fn get(&self) -> &[f64; BANDS] {
        &self.0.0
    }
}

/// Light arriving at a surface, per band. Magnitude is unconstrained.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Illuminant(Spectrum);

impl Illuminant {
    /// Validating constructor: every band is finite and non-negative.
    /// type-audit: bare-ok(constructor-edge: bands)
    pub fn new(bands: [f64; BANDS]) -> Result<Self, UnitError> {
        for value in bands {
            if !value.is_finite() || value < 0.0 {
                return Err(UnitError {
                    unit: "illuminant",
                    value,
                    reason: "every band must be finite and non-negative",
                });
            }
        }
        Ok(Self(Spectrum(bands)))
    }

    /// The raw per-band radiances.
    /// type-audit: bare-ok(constructor-edge: return)
    pub fn get(&self) -> &[f64; BANDS] {
        &self.0.0
    }
}

/// Several reflectances present together, with their weights.
///
/// Combining colours is three unrelated operations, and only one of them is
/// implemented here:
///
/// - **Area** (this type's [`Mixture::integrate`]): components below the
///   resolution you are looking at — speckled granite from ten metres.
///   Separate reflections average on the way to the eye, so the law is the
///   weighted *arithmetic* mean.
/// - **Additive** (not implemented; arrives with multi-light): two torches
///   on one wall. Sum the illuminants, not the reflectances.
/// - **Subtractive** (not implemented; arrives with alchemy): two dyes in
///   one vat. Light passes through both absorbers in series, so the law is
///   the weighted *geometric* mean. This is why blue and yellow paint make
///   green while blue and yellow light make white.
///
/// The producer returns a `Mixture` rather than a finished [`Reflectance`]
/// so a later texture layer — which needs to arrange the components
/// spatially — does not have to re-derive components that were computed and
/// thrown away.
#[derive(Debug, Clone, PartialEq)]
pub struct Mixture {
    components: Vec<Reflectance>,
    weights: Vec<f64>,
}

impl Mixture {
    /// Validating constructor: equal lengths, non-empty, every weight
    /// finite and non-negative, and a strictly positive total.
    /// type-audit: bare-ok(ratio: weights)
    pub fn new(components: Vec<Reflectance>, weights: Vec<f64>) -> Result<Self, UnitError> {
        if components.is_empty() || components.len() != weights.len() {
            return Err(UnitError {
                unit: "mixture",
                value: weights.len() as f64,
                reason: "components and weights must be non-empty and equal in length",
            });
        }
        let mut total = 0.0;
        for w in &weights {
            if !w.is_finite() || *w < 0.0 {
                return Err(UnitError {
                    unit: "mixture",
                    value: *w,
                    reason: "every weight must be finite and non-negative",
                });
            }
            total += *w;
        }
        if total <= 0.0 {
            return Err(UnitError {
                unit: "mixture",
                value: total,
                reason: "total weight must be strictly positive",
            });
        }
        Ok(Self {
            components,
            weights,
        })
    }

    /// The component reflectances, in construction order.
    ///
    /// This accessor and [`Mixture::weights`] are what make the type's
    /// stated purpose real: without them a `Mixture` could only ever be
    /// integrated, the components would be unreachable from another crate,
    /// and the texture layer would have to re-derive exactly what was
    /// already computed and discarded. They are deliberately present before
    /// a consumer exists, because the producer in the terrain domain
    /// returns a `Mixture` across a crate boundary.
    pub fn components(&self) -> &[Reflectance] {
        &self.components
    }

    /// The component weights, in construction order and **unnormalized** —
    /// as passed to [`Mixture::new`]. [`Mixture::integrate`] normalizes
    /// internally; a caller comparing weights against each other should
    /// divide by their sum itself.
    /// type-audit: bare-ok(ratio: return)
    pub fn weights(&self) -> &[f64] {
        &self.weights
    }

    /// Collapse to a single reflectance by the **area** law: the weighted
    /// arithmetic mean, per band. Weights are normalized here, so callers
    /// may pass unnormalized proportions.
    ///
    /// Summation order is the component order, which is fixed by
    /// construction — that is what makes this bit-identical across
    /// platforms. The accumulate is a plain `+=` (an unfused `fadd`), never
    /// `mul_add`: both are IEEE-exact but they round differently from each
    /// other, and mixing the two forms across the workspace would be a
    /// silent cross-platform byte-identity hazard.
    pub fn integrate(&self) -> Reflectance {
        let mut total = 0.0;
        for w in &self.weights {
            total += *w;
        }
        let mut out = [0.0f64; BANDS];
        for (component, weight) in self.components.iter().zip(&self.weights) {
            let share = weight / total;
            for (accumulated, band) in out.iter_mut().zip(component.get()) {
                *accumulated += band * share;
            }
        }
        // Normalized weights sum to 1 and every component band is within
        // [0, 1], so the mean is too — but clamp against accumulated
        // rounding so the constructor can never reject our own output.
        for value in out.iter_mut() {
            *value = value.clamp(0.0, 1.0);
        }
        Reflectance(Spectrum(out))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_spectrum_rejects_a_non_finite_band() {
        let mut v = [0.5; BANDS];
        v[3] = f64::NAN;
        assert!(Spectrum::new(v).is_err());
    }

    #[test]
    fn a_reflectance_rejects_a_band_above_one() {
        let mut v = [0.5; BANDS];
        v[0] = 1.5;
        let err = Reflectance::new(v).unwrap_err();
        assert_eq!(err.unit, "reflectance");
    }

    #[test]
    fn a_reflectance_rejects_a_negative_band() {
        let mut v = [0.5; BANDS];
        v[9] = -0.01;
        assert!(Reflectance::new(v).is_err());
    }

    #[test]
    fn area_mixing_lands_between_its_components_in_every_band() {
        let dark = Reflectance::new([0.1; BANDS]).unwrap();
        let bright = Reflectance::new([0.9; BANDS]).unwrap();
        let mixed = Mixture::new(vec![dark, bright], vec![0.25, 0.75])
            .unwrap()
            .integrate();
        for b in 0..BANDS {
            assert!(
                mixed.get()[b] > 0.1 && mixed.get()[b] < 0.9,
                "band {b} = {} escaped its components",
                mixed.get()[b]
            );
        }
    }

    #[test]
    fn area_mixing_is_the_weighted_arithmetic_mean() {
        // Every value here is a dyadic rational, so the whole computation
        // is EXACT in binary floating point and `assert_eq!` witnesses
        // bit-exactness rather than approximate agreement — which is the
        // property this crate exists to defend.
        //
        // Do not "simplify" these to round decimals like 0.2 and 0.6: those
        // are inexact in binary, each product rounds, the sum rounds again,
        // and the result lands one ULP low. The only arithmetic that
        // recovers the tidy answer is a fused `mul_add`, which this module
        // forbids — so tidy-looking decimals here would silently become a
        // test that the implementation IS fused.
        let a = Reflectance::new([0.25; BANDS]).unwrap();
        let b = Reflectance::new([0.75; BANDS]).unwrap();
        let mixed = Mixture::new(vec![a, b], vec![0.25, 0.75])
            .unwrap()
            .integrate();
        // 0.25*0.25 + 0.75*0.75 = 0.0625 + 0.5625 = 0.625, exactly.
        assert_eq!(mixed.get()[0], 0.625);
    }

    #[test]
    fn a_mixture_normalizes_its_weights() {
        // Dyadic values again, for the reason given on
        // `area_mixing_is_the_weighted_arithmetic_mean`. 1/4 and 3/4 are
        // themselves exact, so normalization introduces no rounding either.
        let a = Reflectance::new([0.25; BANDS]).unwrap();
        let b = Reflectance::new([0.75; BANDS]).unwrap();
        // Weights 1 and 3 are the same mixture as 0.25 and 0.75.
        let mixed = Mixture::new(vec![a, b], vec![1.0, 3.0])
            .unwrap()
            .integrate();
        assert_eq!(mixed.get()[0], 0.625);
    }

    #[test]
    fn a_mixture_rejects_mismatched_lengths() {
        let a = Reflectance::new([0.2; BANDS]).unwrap();
        assert!(Mixture::new(vec![a], vec![1.0, 2.0]).is_err());
    }

    #[test]
    fn a_mixture_rejects_zero_total_weight() {
        let a = Reflectance::new([0.2; BANDS]).unwrap();
        let b = Reflectance::new([0.6; BANDS]).unwrap();
        assert!(Mixture::new(vec![a, b], vec![0.0, 0.0]).is_err());
    }

    #[test]
    fn mixing_never_exceeds_unit_reflectance() {
        let a = Reflectance::new([1.0; BANDS]).unwrap();
        let b = Reflectance::new([1.0; BANDS]).unwrap();
        let mixed = Mixture::new(vec![a, b], vec![0.5, 0.5])
            .unwrap()
            .integrate();
        for b in 0..BANDS {
            assert!(mixed.get()[b] <= 1.0, "band {b} broke energy conservation");
        }
    }

    #[test]
    fn area_mixing_does_not_fuse_its_multiply_and_add() {
        // The guard for the workspace-wide rule that `a * b + c` and
        // `a.mul_add(b, c)` are never mixed. Both are IEEE-exact, but they
        // round differently from each other, so mixing the two forms is a
        // silent cross-platform byte-identity hazard.
        //
        // These inputs are chosen because they DISTINGUISH the two: 0.2 and
        // 0.6 are inexact in binary, so the unfused form rounds the product
        // and then rounds the sum, landing one ULP below 0.5, while a fused
        // multiply-add rounds once and reaches 0.5 exactly. The literal
        // below is therefore a fingerprint of the unfused implementation —
        // if this test starts failing with `right: 0.5`, someone introduced
        // a `mul_add`.
        //
        // (Its sibling `area_mixing_is_the_weighted_arithmetic_mean` uses
        // dyadic inputs so it can state the LAW without depending on
        // rounding at all. The two tests answer different questions and
        // both are needed: that one would pass under either form.)
        let a = Reflectance::new([0.2; BANDS]).unwrap();
        let b = Reflectance::new([0.6; BANDS]).unwrap();
        let mixed = Mixture::new(vec![a, b], vec![0.25, 0.75])
            .unwrap()
            .integrate();
        assert_eq!(mixed.get()[0], 0.499_999_999_999_999_94);
    }

    #[test]
    fn a_mixture_keeps_its_components_reachable() {
        // The type's whole reason for existing instead of a bare
        // Reflectance: a later texture layer, in another crate, needs the
        // components to arrange them spatially. Without these accessors the
        // doc comment above would be a promise the API cannot keep.
        let a = Reflectance::new([0.25; BANDS]).unwrap();
        let b = Reflectance::new([0.75; BANDS]).unwrap();
        let m = Mixture::new(vec![a, b], vec![1.0, 3.0]).unwrap();
        assert_eq!(m.components().len(), 2);
        assert_eq!(m.components()[0].get()[0], 0.25);
        assert_eq!(m.components()[1].get()[0], 0.75);
        // Weights come back as passed, unnormalized.
        assert_eq!(m.weights(), &[1.0, 3.0]);
    }

    #[test]
    fn the_band_grid_is_ten_uniform_forty_nanometre_bands() {
        assert_eq!(BANDS, 10);
        assert_eq!(BAND_CENTERS_NM[0], 360.0);
        assert_eq!(BAND_CENTERS_NM[BANDS - 1], 720.0);
        for b in 1..BANDS {
            assert_eq!(BAND_CENTERS_NM[b] - BAND_CENTERS_NM[b - 1], 40.0);
        }
    }
}
