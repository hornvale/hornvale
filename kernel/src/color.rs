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

/// One number per observer channel — the collapse of an entire arriving
/// spectrum down to what an eye actually transmits.
#[derive(Debug, Clone, PartialEq)]
pub struct Signal(Vec<f64>);

impl Signal {
    /// The raw per-channel responses.
    /// type-audit: bare-ok(constructor-edge: return)
    pub fn get(&self) -> &[f64] {
        &self.0
    }

    /// Squared Euclidean distance in signal space. Squared rather than
    /// rooted because every caller only ranks distances, and `sqrt` would
    /// be a monotone transform that buys nothing.
    ///
    /// Signals of differing length compare as [`f64::INFINITY`] — they come
    /// from different observers and are not comparable at all.
    /// type-audit: bare-ok(ratio: return)
    pub fn distance_to(&self, other: &Signal) -> f64 {
        if self.0.len() != other.0.len() {
            return f64::INFINITY;
        }
        let mut sum = 0.0;
        for (a, b) in self.0.iter().zip(&other.0) {
            let d = a - b;
            sum += d * d;
        }
        sum
    }
}

/// An eye: one sensitivity curve per channel. Humans have three photopic
/// channels plus rods; other creatures have other counts, which is the
/// whole reason the channel set is a `Vec` and not an array.
#[derive(Debug, Clone, PartialEq)]
pub struct Observer {
    channels: Vec<Spectrum>,
    /// Whether this observer's signal has a real (non-false-colour) sRGB
    /// image. True only for [`standard_observer`].
    srgb_native: bool,
}

impl Observer {
    /// Validating constructor: at least one channel.
    ///
    /// An observer built this way is **not** sRGB-native: [`to_srgb`]
    /// returns `None`, because a signal from an arbitrary channel set has
    /// no truthful three-channel image and any mapping would be a
    /// false-colour choice the caller must declare (RENDER-9).
    ///
    /// [`to_srgb`]: Observer::to_srgb
    pub fn new(channels: Vec<Spectrum>) -> Result<Self, UnitError> {
        if channels.is_empty() {
            return Err(UnitError {
                unit: "observer",
                value: 0.0,
                reason: "an observer needs at least one channel",
            });
        }
        Ok(Self {
            channels,
            srgb_native: false,
        })
    }

    /// How many channels this observer has.
    /// type-audit: bare-ok(count: return)
    pub fn channels(&self) -> usize {
        self.channels.len()
    }

    /// The three-way product: `signal[c] = Σ_b r[b] · i[b] · s[c][b]`.
    ///
    /// Multiplication and addition only, over fixed-size arrays in a fixed
    /// order — IEEE 754 requires both to be exact, so this is bit-identical
    /// on every platform without routing through [`crate::math`]. The
    /// accumulate is a plain `+=` (an unfused `fadd`), never `mul_add`, for
    /// the reason given on [`Mixture::integrate`].
    pub fn sense(&self, reflectance: &Reflectance, illuminant: &Illuminant) -> Signal {
        let r = reflectance.get();
        let i = illuminant.get();
        let mut out = Vec::with_capacity(self.channels.len());
        for channel in &self.channels {
            let s = channel.get();
            let mut sum = 0.0;
            for ((r_b, i_b), s_b) in r.iter().zip(i.iter()).zip(s.iter()) {
                sum += r_b * i_b * s_b;
            }
            out.push(sum);
        }
        Signal(out)
    }

    /// Project a signal to display bytes, or `None` when this observer has
    /// no truthful sRGB image.
    ///
    /// Only [`standard_observer`] is sRGB-native. For anything else the
    /// answer is `None` on purpose: the caller must choose and *caption* a
    /// false-colour mapping rather than have one invented here, because the
    /// caption — not the picture — carries the honesty (RENDER-9).
    /// type-audit: bare-ok(artifact: return)
    pub fn to_srgb(&self, signal: &Signal) -> Option<[u8; 3]> {
        if !self.srgb_native || signal.get().len() != 4 {
            return None;
        }
        // Channel order is [short, medium, long, scotopic]; the scotopic
        // channel carries no hue and is not projected. The photopic
        // channels are normalized by the response a unit-reflectance
        // surface under a unit illuminant would produce, so a white surface
        // under flat light lands at white rather than at an arbitrary
        // scale.
        let s = signal.get();
        let mut out = [0u8; 3];
        // Long → red, medium → green, short → blue.
        for (slot, (raw, norm)) in
            out.iter_mut()
                .zip([(s[2], LONG_NORM), (s[1], MEDIUM_NORM), (s[0], SHORT_NORM)])
        {
            let linear = (raw / norm).clamp(0.0, 1.0);
            *slot = encode_srgb_byte(linear);
        }
        Some(out)
    }
}

/// Normalizing constants: the response each photopic channel gives to a
/// unit-reflectance surface under a unit illuminant. Derived from
/// [`standard_observer`]'s own curves, so the two cannot drift apart —
/// `standard_observer_channels_sum_to_the_declared_norms` proves it.
/// type-audit: bare-ok(ratio)
const SHORT_NORM: f64 = 1.98;
/// See [`SHORT_NORM`].
/// type-audit: bare-ok(ratio)
const MEDIUM_NORM: f64 = 3.51;
/// See [`SHORT_NORM`].
/// type-audit: bare-ok(ratio)
const LONG_NORM: f64 = 3.95;

/// Encode a linear `[0, 1]` intensity as an sRGB byte.
///
/// This is the one transcendental in the colour path, and it sits at the
/// emit boundary rather than in the hot loop. It routes through
/// [`crate::math::powf`] like every other transcendental in the workspace
/// (decision 0041).
/// type-audit: bare-ok(artifact: return)
fn encode_srgb_byte(linear: f64) -> u8 {
    let encoded = if linear <= 0.003_130_8 {
        12.92 * linear
    } else {
        1.055 * crate::math::powf(linear, 1.0 / 2.4) - 0.055
    };
    (encoded.clamp(0.0, 1.0) * 255.0).round() as u8
}

/// The human-calibrated observer: three photopic channels (short, medium,
/// long) plus one scotopic rod-like channel used at low light.
///
/// The curves are coarse samples of human cone and rod sensitivity on the
/// band grid. They are approximations and say so — the campaign's claims
/// rest on *differences between observers*, not on colorimetric accuracy.
pub fn standard_observer() -> Observer {
    let short = Spectrum([0.00, 0.25, 1.00, 0.62, 0.10, 0.01, 0.00, 0.00, 0.00, 0.00]);
    let medium = Spectrum([0.00, 0.01, 0.10, 0.45, 0.90, 1.00, 0.72, 0.28, 0.05, 0.00]);
    let long = Spectrum([0.00, 0.01, 0.06, 0.25, 0.60, 0.92, 1.00, 0.75, 0.30, 0.06]);
    let scotopic = Spectrum([0.00, 0.15, 0.55, 0.95, 1.00, 0.68, 0.25, 0.05, 0.00, 0.00]);
    Observer {
        channels: vec![short, medium, long, scotopic],
        srgb_native: true,
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

    /// A flat unit illuminant — every band equal. Used wherever a test
    /// wants reflectance differences and no illuminant differences.
    fn flat_light() -> Illuminant {
        Illuminant::new([1.0; BANDS]).unwrap()
    }

    #[test]
    fn the_standard_observer_has_four_channels() {
        // Three photopic plus one scotopic (rod-like) channel.
        assert_eq!(standard_observer().channels(), 4);
    }

    #[test]
    fn a_brighter_surface_produces_a_larger_signal_in_every_channel() {
        let obs = standard_observer();
        let dim = obs.sense(&Reflectance::new([0.2; BANDS]).unwrap(), &flat_light());
        let bright = obs.sense(&Reflectance::new([0.8; BANDS]).unwrap(), &flat_light());
        for c in 0..obs.channels() {
            assert!(
                bright.get()[c] > dim.get()[c],
                "channel {c}: {} was not brighter than {}",
                bright.get()[c],
                dim.get()[c]
            );
        }
    }

    #[test]
    fn naming_works_at_a_channel_count_below_the_standard() {
        // A synthetic two-channel dichromat: one short-biased channel, one
        // long-biased. Proves the pipeline is not hardcoded to four.
        let short = Spectrum::new([1.0, 1.0, 1.0, 0.5, 0.2, 0.0, 0.0, 0.0, 0.0, 0.0]).unwrap();
        let long = Spectrum::new([0.0, 0.0, 0.0, 0.0, 0.2, 0.5, 1.0, 1.0, 1.0, 1.0]).unwrap();
        let obs = Observer::new(vec![short, long]).unwrap();
        assert_eq!(obs.channels(), 2);

        let bluish =
            Reflectance::new([0.8, 0.8, 0.8, 0.6, 0.2, 0.05, 0.05, 0.05, 0.05, 0.05]).unwrap();
        let s = obs.sense(&bluish, &flat_light());
        assert_eq!(s.get().len(), 2);
        assert!(
            s.get()[0] > s.get()[1],
            "a short-biased surface must excite the short channel more"
        );
    }

    #[test]
    fn naming_works_at_a_channel_count_above_the_standard() {
        // A synthetic five-channel observer, each channel a single band.
        // Counts either side of the standard's four mean neither of these
        // two tests can pass by accidentally exercising the standard path.
        let mut channels = Vec::new();
        for b in 0..5 {
            let mut curve = [0.0; BANDS];
            curve[b] = 1.0;
            channels.push(Spectrum::new(curve).unwrap());
        }
        let obs = Observer::new(channels).unwrap();
        assert_eq!(obs.channels(), 5);
        let s = obs.sense(&Reflectance::new([0.5; BANDS]).unwrap(), &flat_light());
        assert_eq!(s.get().len(), 5);
    }

    #[test]
    fn an_observer_rejects_an_empty_channel_set() {
        assert!(Observer::new(vec![]).is_err());
    }

    #[test]
    fn sensing_is_bit_identical_across_repeated_calls() {
        let obs = standard_observer();
        let r = Reflectance::new([0.37; BANDS]).unwrap();
        let a = obs.sense(&r, &flat_light());
        let b = obs.sense(&r, &flat_light());
        assert_eq!(a.get(), b.get());
    }

    #[test]
    fn signal_distance_is_zero_for_identical_signals_and_positive_otherwise() {
        let obs = standard_observer();
        let a = obs.sense(&Reflectance::new([0.3; BANDS]).unwrap(), &flat_light());
        let b = obs.sense(&Reflectance::new([0.7; BANDS]).unwrap(), &flat_light());
        assert_eq!(a.distance_to(&a), 0.0);
        assert!(a.distance_to(&b) > 0.0);
    }

    #[test]
    fn the_standard_observer_projects_to_srgb_but_a_synthetic_one_does_not() {
        let obs = standard_observer();
        let s = obs.sense(&Reflectance::new([0.5; BANDS]).unwrap(), &flat_light());
        assert!(
            obs.to_srgb(&s).is_some(),
            "the standard observer has a real mapping"
        );

        // A five-channel signal has no truthful sRGB image. Any mapping
        // would be a false-colour decision, and RENDER-9 requires that be
        // declared by the caller rather than invented here.
        let mut channels = Vec::new();
        for b in 0..5 {
            let mut curve = [0.0; BANDS];
            curve[b] = 1.0;
            channels.push(Spectrum::new(curve).unwrap());
        }
        let alien = Observer::new(channels).unwrap();
        let alien_signal = alien.sense(&Reflectance::new([0.5; BANDS]).unwrap(), &flat_light());
        assert!(alien.to_srgb(&alien_signal).is_none());
    }

    #[test]
    fn a_white_surface_under_flat_light_projects_near_white() {
        let obs = standard_observer();
        let s = obs.sense(&Reflectance::new([1.0; BANDS]).unwrap(), &flat_light());
        let [r, g, b] = obs.to_srgb(&s).unwrap();
        for channel in [r, g, b] {
            assert!(channel > 200, "expected a bright neutral, got {r},{g},{b}");
        }
    }

    #[test]
    fn standard_observer_channels_sum_to_the_declared_norms() {
        let obs = standard_observer();
        let sums: Vec<f64> = obs
            .channels
            .iter()
            .map(|c| c.get().iter().sum::<f64>())
            .collect();
        // Rounded to two places: the constants are the normalizers used by
        // to_srgb, and a curve edit that does not update them would make a
        // white surface stop projecting to white.
        assert_eq!((sums[0] * 100.0).round() / 100.0, SHORT_NORM);
        assert_eq!((sums[1] * 100.0).round() / 100.0, MEDIUM_NORM);
        assert_eq!((sums[2] * 100.0).round() / 100.0, LONG_NORM);
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
