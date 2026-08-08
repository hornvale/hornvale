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
//! exact (decision 0041). No `math.rs` call appears in it. Use `a * b + c`
//! and never `mul_add`: both are exact but they round differently from
//! each other. The one transcendental in this module is [`planck_relative`]'s
//! exponential, which routes through `math.rs` like every other.

use crate::math;
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

/// The width of one band, nanometres. The grid is ten uniform bands whose
/// edges span 340–740, so band `i` covers
/// `BAND_CENTERS_NM[i] ± BAND_WIDTH_NM / 2`.
/// type-audit: bare-ok(ratio)
pub const BAND_WIDTH_NM: f64 = 40.0;

/// Planck's second radiation constant, `hc/k`, in nanometre-kelvin. Used in
/// the exponential term of the spectral radiance law.
/// type-audit: bare-ok(ratio)
const C2_NM_K: f64 = 1.438_776_877e7;

/// Spectral radiance of a blackbody at `t_kelvin`, at `wavelength_nm`, up to
/// a constant factor. The leading `c1` is omitted because every consumer
/// works in ratios or renormalizes — carrying it would only scale all ten
/// bands together.
///
/// **This lives in the kernel, not in astronomy, because it takes no
/// world-state.** A star, a hearth and a forge are the same law at three
/// temperatures; the temperature is the datum and belongs to whoever owns
/// the thing, but the law is substrate. (Astronomy's `at_elevation` stays in
/// astronomy for the mirror-image reason: it is parameterized by a sun's
/// elevation.)
/// type-audit: bare-ok(ratio: wavelength_nm), bare-ok(ratio: t_kelvin), bare-ok(ratio: return)
pub fn planck_relative(wavelength_nm: f64, t_kelvin: f64) -> f64 {
    let l5 = wavelength_nm.powi(5);
    let x = C2_NM_K / (wavelength_nm * t_kelvin);
    1.0 / (l5 * (math::exp(x) - 1.0))
}

/// Simpson nodes per band. **A permanent contract**: changing it moves every
/// colour in the world. Chosen by measurement, not taste (spec §5.2) — 13
/// nodes hold at least 20x below a `u8` quantization step down to 700 K, a
/// dull red glow colder than anything the project names, so a later ember or
/// forge cannot force it to change. Five nodes already fails by 900 K.
/// Must be odd.
/// type-audit: bare-ok(count)
const BAND_NODES: usize = 13;

/// The mean of `f` across the band centred at `center_nm`, by Simpson's rule.
fn band_mean(center_nm: f64, f: &dyn Fn(f64) -> f64) -> f64 {
    band_mean_with_nodes(center_nm, BAND_NODES, f)
}

/// [`band_mean`] at an explicit node count, so a test can compare the shipped
/// count against a converged reference. Not public: the node count is a
/// contract, not a caller's choice.
fn band_mean_with_nodes(center_nm: f64, nodes: usize, f: &dyn Fn(f64) -> f64) -> f64 {
    debug_assert!(
        nodes >= 3 && nodes % 2 == 1,
        "Simpson needs an odd node count >= 3"
    );
    let a = center_nm - BAND_WIDTH_NM / 2.0;
    let h = BAND_WIDTH_NM / (nodes - 1) as f64;
    let mut sum = 0.0;
    for i in 0..nodes {
        let weight = if i == 0 || i == nodes - 1 {
            1.0
        } else if i % 2 == 1 {
            4.0
        } else {
            2.0
        };
        sum += weight * f(a + i as f64 * h);
    }
    sum * h / 3.0 / BAND_WIDTH_NM
}

/// A blackbody at `t_kelvin` on the band grid, normalized so the brightest
/// band is 1.0.
///
/// **A band integral, not a midpoint sample.** [`BAND_CENTERS_NM`]'s own doc
/// says anything integrating over a band wants the *edges*. The midpoint
/// rule's error is 0.26 % at 5800 K but 34 % at 1100 K, because below the
/// grid the visible range is the steep, strongly convex Wien tail and a
/// midpoint sample underestimates a convex mean. A star could afford that; a
/// hearth cannot.
///
/// Normalizing here means downstream code compares *colour*, not distance
/// from the source.
/// type-audit: bare-ok(ratio: t_kelvin)
pub fn blackbody(t_kelvin: f64) -> Illuminant {
    let mut bands = [0.0f64; BANDS];
    let mut peak = 0.0f64;
    for (band, center) in bands.iter_mut().zip(BAND_CENTERS_NM.iter()) {
        let value = band_mean(*center, &|nm| planck_relative(nm, t_kelvin));
        *band = value;
        if value > peak {
            peak = value;
        }
    }
    // `peak` is strictly positive for any finite positive temperature, so
    // this division is total; the guard is defensive, not a live path.
    if peak > 0.0 {
        for value in bands.iter_mut() {
            *value /= peak;
        }
    }
    Illuminant::new(bands).expect("a normalized Planck curve is finite and non-negative")
}

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

/// What a channel contributes to sight.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ChannelRole {
    /// Carries hue. A projection may read it; chromaticity counts it.
    Chromatic,
    /// Carries brightness only. A real eye has such channels (rods), and
    /// their signal cannot be told from intensity — so no projection reads
    /// one and no chromaticity metric counts one.
    Achromatic,
}

/// A named way of putting a signal on a three-channel screen, and what it
/// preserves.
///
/// Named after the discipline that already solved this problem: every map
/// projection is a lie, and cartography's answer is not to find a true one —
/// it is to name the projection on the map and say which invariant it
/// preserves.
#[derive(Debug, Clone, PartialEq)]
pub struct Projection {
    /// The registered name, e.g. "native" or "yellow-blue".
    name: &'static str,
    /// What survives the projection. The caption's load-bearing half.
    preserves: &'static str,
    /// Which channel drives R, G, B. Every index must name a `Chromatic`
    /// channel on the observer that carries this projection — that check
    /// happens in [`Observer::with_roles`], not here, because roles live on
    /// the observer and a `Projection` is built before one is attached.
    /// type-audit: bare-ok(index)
    rgb: [usize; 3],
    /// Per-channel normalizer: the response a unit-reflectance surface
    /// under a unit illuminant gives on that channel, indexed by *output
    /// slot* (not by channel). **Carried, not derived** — this is a
    /// byte-identity requirement, not a style choice: the shipped constants
    /// for [`standard_observer`] are the *rounded* channel sums, so
    /// deriving them live from the curves would move every colour the
    /// standard observer has ever emitted.
    /// type-audit: bare-ok(ratio)
    norms: [f64; 3],
}

impl Projection {
    /// Validating constructor: every normalizer must be finite and
    /// non-zero (a zero normalizer would divide every signal by zero).
    ///
    /// This cannot validate `rgb` against channel roles — roles live on the
    /// [`Observer`], not the projection, so a `Projection` can be built
    /// before any observer exists. That check belongs to and happens in
    /// [`Observer::with_roles`].
    /// type-audit: bare-ok(identifier-text: name), bare-ok(identifier-text: preserves), bare-ok(index: rgb), bare-ok(ratio: norms)
    pub fn new(
        name: &'static str,
        preserves: &'static str,
        rgb: [usize; 3],
        norms: [f64; 3],
    ) -> Result<Self, UnitError> {
        for n in norms {
            if !n.is_finite() || n == 0.0 {
                return Err(UnitError {
                    unit: "projection",
                    value: n,
                    reason: "every normalizer must be finite and non-zero",
                });
            }
        }
        Ok(Self {
            name,
            preserves,
            rgb,
            norms,
        })
    }

    /// The registered name.
    /// type-audit: bare-ok(identifier-text: return)
    pub fn name(&self) -> &'static str {
        self.name
    }

    /// What this projection preserves.
    /// type-audit: bare-ok(identifier-text: return)
    pub fn preserves(&self) -> &'static str {
        self.preserves
    }

    /// The per-output-slot normalizers, exactly as carried — bit-exact, not
    /// re-derived. This is what lets a caller (or a test) tell a *carried*
    /// constant from a *derived* one before either has been rounded through
    /// `to_srgb`'s `u8` output, which can absorb a difference this
    /// accessor cannot.
    /// type-audit: bare-ok(ratio: return)
    pub fn norms(&self) -> &[f64; 3] {
        &self.norms
    }
}

/// An eye: one sensitivity curve per channel. Humans have three photopic
/// channels plus rods; other creatures have other counts, which is the
/// whole reason the channel set is a `Vec` and not an array.
#[derive(Debug, Clone, PartialEq)]
pub struct Observer {
    channels: Vec<Spectrum>,
    /// What each channel contributes to sight, one per channel.
    roles: Vec<ChannelRole>,
    /// This observer's declared way of putting a signal on a screen, if it
    /// has one. `None` for anything built with [`Observer::new`].
    projection: Option<Projection>,
}

impl Observer {
    /// Validating constructor: at least one channel.
    ///
    /// An observer built this way has **no projection**: [`to_srgb`]
    /// returns `None`, because a signal from an arbitrary channel set has
    /// no truthful three-channel image and any mapping would be a
    /// false-colour choice the caller must declare (RENDER-9). Every
    /// channel is [`ChannelRole::Chromatic`] — use [`Observer::with_roles`]
    /// to declare an achromatic channel or attach a projection.
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
        let roles = vec![ChannelRole::Chromatic; channels.len()];
        Ok(Self {
            channels,
            roles,
            projection: None,
        })
    }

    /// Validating constructor: one role per channel, at least one
    /// `Chromatic` channel, and — when a projection is present — every
    /// `rgb` index in range and naming a `Chromatic` channel.
    ///
    /// This is the constructor that lets an eye declare a rod (an
    /// [`ChannelRole::Achromatic`] channel) and, optionally, a real sRGB
    /// image via a [`Projection`]. Without the index check, a projection
    /// could silently read an achromatic channel and show brightness as a
    /// hue.
    pub fn with_roles(
        channels: Vec<Spectrum>,
        roles: Vec<ChannelRole>,
        projection: Option<Projection>,
    ) -> Result<Self, UnitError> {
        if channels.is_empty() || channels.len() != roles.len() {
            return Err(UnitError {
                unit: "observer",
                value: roles.len() as f64,
                reason: "an observer needs one role per channel and at least one channel",
            });
        }
        if !roles.contains(&ChannelRole::Chromatic) {
            return Err(UnitError {
                unit: "observer",
                value: roles.len() as f64,
                reason: "an observer needs at least one chromatic channel",
            });
        }
        if let Some(p) = &projection {
            for idx in p.rgb {
                if !matches!(roles.get(idx), Some(ChannelRole::Chromatic)) {
                    return Err(UnitError {
                        unit: "observer",
                        value: idx as f64,
                        reason: "a projection may only name an in-range chromatic channel",
                    });
                }
            }
        }
        Ok(Self {
            channels,
            roles,
            projection,
        })
    }

    /// How many channels this observer has.
    /// type-audit: bare-ok(count: return)
    pub fn channels(&self) -> usize {
        self.channels.len()
    }

    /// The role of each channel, in channel order.
    pub fn roles(&self) -> &[ChannelRole] {
        &self.roles
    }

    /// This observer's declared projection, if it has one.
    pub fn projection(&self) -> Option<&Projection> {
        self.projection.as_ref()
    }

    /// How many channels are [`ChannelRole::Chromatic`] — the length of
    /// every [`Observer::chromaticity`] result.
    /// type-audit: bare-ok(count: return)
    pub fn chromatic_channels(&self) -> usize {
        self.roles
            .iter()
            .filter(|r| **r == ChannelRole::Chromatic)
            .count()
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

    /// Each `Chromatic` channel's share of the chromatic total — the
    /// discriminability substrate. An `Achromatic` channel (a rod) is
    /// excluded from both the numerator and the denominator, so a louder
    /// rod can never move a chromaticity: before roles existed, the rod
    /// carried hue information and every observer with one measured as a
    /// full trichromat regardless of its photopic channel count.
    ///
    /// A zero chromatic total (e.g. total darkness) returns an all-zero
    /// vector rather than propagating `NaN`.
    /// type-audit: bare-ok(ratio: return)
    pub fn chromaticity(&self, signal: &Signal) -> Vec<f64> {
        let s = signal.get();
        let mut total = 0.0;
        for (value, role) in s.iter().zip(&self.roles) {
            if *role == ChannelRole::Chromatic {
                total += *value;
            }
        }
        let mut out = Vec::with_capacity(self.chromatic_channels());
        for (value, role) in s.iter().zip(&self.roles) {
            if *role == ChannelRole::Chromatic {
                out.push(if total == 0.0 { 0.0 } else { value / total });
            }
        }
        out
    }

    /// Squared Euclidean distance between two signals' chromaticities — the
    /// discriminability metric. Squared for the reason given on
    /// [`Signal::distance_to`].
    ///
    /// Signals of differing length compare as [`f64::INFINITY`], the same
    /// posture [`Signal::distance_to`] takes: they come from different
    /// observers and are not comparable at all.
    /// type-audit: bare-ok(ratio: return)
    pub fn chromatic_distance(&self, a: &Signal, b: &Signal) -> f64 {
        if a.get().len() != self.channels.len() || b.get().len() != self.channels.len() {
            return f64::INFINITY;
        }
        let ca = self.chromaticity(a);
        let cb = self.chromaticity(b);
        let mut sum = 0.0;
        for (x, y) in ca.iter().zip(&cb) {
            let d = x - y;
            sum += d * d;
        }
        sum
    }

    /// Project a signal to display bytes, or `None` when this observer has
    /// no declared [`Projection`].
    ///
    /// Only observers carrying a projection ([`standard_observer`], or
    /// anything built via [`Observer::with_roles`] with one) have a
    /// truthful three-channel image. For anything else the answer is
    /// `None` on purpose: the caller must choose and *caption* a
    /// false-colour mapping rather than have one invented here, because the
    /// caption — not the picture — carries the honesty (RENDER-9).
    /// # The scotopic term
    ///
    /// Below [`PHOTOPIC_THRESHOLD`] the cones cannot round a single count
    /// onto the screen, and the image becomes the achromatic channels' —
    /// contributed **equally to all three slots**, which is grey sight, which
    /// is what night vision is. A rod carries no hue and so can never shift
    /// one; that is the same fact [`ChannelRole::Achromatic`] states, now
    /// cashed rather than merely respected. Before this, no projection read
    /// the rod at all and night vision could not reach the screen: a human
    /// and a kobold went black together while their rod responses genuinely
    /// differed.
    ///
    /// The rod fades in as the cone image fades out (`fade` runs 0 at the
    /// threshold to 1 in true darkness), so the handover is continuous rather
    /// than a pop, and rod saturation as light rises is modelled by the same
    /// factor running the other way.
    ///
    /// **The term is exactly zero at and above the threshold** — the branch
    /// returns the photopic bytes untouched, not the photopic bytes plus a
    /// small number — so every colour already committed is bit-identical
    /// (spec §6 H3).
    /// type-audit: bare-ok(artifact: return)
    pub fn to_srgb(&self, signal: &Signal) -> Option<[u8; 3]> {
        let photopic = self.photopic_slots(signal)?;
        let level = photopic.iter().copied().fold(0.0f64, f64::max);
        if level >= PHOTOPIC_THRESHOLD {
            return Some([
                encode_srgb_byte(photopic[0]),
                encode_srgb_byte(photopic[1]),
                encode_srgb_byte(photopic[2]),
            ]);
        }
        let fade = (PHOTOPIC_THRESHOLD - level) / PHOTOPIC_THRESHOLD;
        let mut rod = 0.0;
        for (value, role) in signal.get().iter().zip(&self.roles) {
            if *role == ChannelRole::Achromatic {
                rod += *value;
            }
        }
        let scotopic = (SCOTOPIC_GAIN * rod / SCOTOPIC_NORM).clamp(0.0, 1.0);
        let grey = encode_srgb_byte(fade * scotopic);
        Some([grey; 3])
    }

    /// The linear intensity each output slot's **cone** channel carries,
    /// clamped to the displayable range — the photopic image, before
    /// quantization to bytes and before any scotopic term.
    ///
    /// Extracted so the projection is written down once: the scotopic term
    /// needs to know how bright the cone image is in order to decide whether
    /// it applies at all, and a second copy of `s[idx] / norm` would be a
    /// second opinion about the rgb/norms crossed ordering
    /// (`standard_observer`'s own comment names that as the most likely bug
    /// in this area).
    ///
    /// `None` for an observer with no [`Projection`], or a signal from a
    /// different channel set — the same two refusals [`Observer::to_srgb`]
    /// makes, for the same reasons.
    fn photopic_slots(&self, signal: &Signal) -> Option<[f64; 3]> {
        let p = self.projection.as_ref()?;
        if signal.get().len() != self.channels.len() {
            return None;
        }
        let s = signal.get();
        let mut out = [0.0f64; 3];
        for (slot, (idx, norm)) in out.iter_mut().zip(p.rgb.iter().zip(p.norms.iter())) {
            *slot = (s[*idx] / norm).clamp(0.0, 1.0);
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

/// The rod's normalizer: the response the standard observer's scotopic
/// channel gives to a unit-reflectance surface under a unit illuminant.
///
/// **Carried, and deliberately the STANDARD rod's — not the observer's own.**
/// Every derived eye's rod is the standard scotopic curve times that species'
/// `scotopic_gain` (`hornvale_worldgen::observer`), so normalizing each
/// observer by *its own* rod sum would divide that gain straight back out and
/// a kobold's night vision would render pixel-for-pixel identical to a
/// human's. The gain is the whole point of the channel; a shared normalizer
/// is what lets it reach the screen.
///
/// `standard_observer_channels_sum_to_the_declared_norms` pins it against the
/// curve, exactly as it pins the three photopic norms.
/// type-audit: bare-ok(ratio)
const SCOTOPIC_NORM: f64 = 3.63;

/// The photopic level — the brightest value the cone channels can put in any
/// output slot — at or above which sight is entirely cone-driven and
/// [`Observer::to_srgb`]'s scotopic term is **exactly zero**.
///
/// **Read off the display, not chosen to make anything come out.**
/// [`encode_srgb_byte`] scales its lower segment by 12.92 and rounds to a
/// byte, so a linear intensity below `0.5 / 255 / 12.92` rounds to byte 0 in
/// every slot: below this level the cones put *nothing at all* on the screen.
/// That is what makes the term safe by construction rather than by luck: any
/// pixel bright enough to show a byte is, by this definition, above the
/// threshold, so the term can only ever fill in a pixel photopic sight had
/// already left black. H3 (spec §6) is therefore a property of where the
/// threshold is *read from*, not a coincidence of the value it happens to
/// take.
///
/// For scale, and measured rather than assumed
/// (`windows/vessel/tests/lantern_night.rs`, the H4a reading): the dimmest
/// cell the shipped game renders across seeds 1, 42, 99 and 256 is
/// `[2, 2, 0]`, a linear `6.07e-4` — four times this threshold, and two
/// bytes clear of the floor. The term is unreachable on the chamber band.
/// type-audit: bare-ok(ratio)
const PHOTOPIC_THRESHOLD: f64 = 0.5 / 255.0 / 12.92;

/// How far the fully dark-adapted rod system is amplified onto the screen.
///
/// **A gain is not optional, and this is why.** The measurement that motivated
/// the whole term (spec §4.4) was a human and a kobold going black *together*
/// in near-darkness while their rod responses genuinely differed — 0.0003
/// against 0.0004. Divided by [`SCOTOPIC_NORM`] those are `8e-5` and `1e-4`,
/// both below [`PHOTOPIC_THRESHOLD`]: at unit gain the rod's image is below
/// one screen count everywhere in its own regime, and the term would ship
/// green and do nothing.
///
/// The magnitude is the physiological one: the dark-adapted rod system is
/// about three log units — a factor of 1000 — more sensitive than the cone
/// system, which is the depth of the dark-adaptation curve. Measured
/// consequence on this model's curves: a limestone wall under a dimmed torch
/// peaks at byte 29 for a human and byte 38 for a kobold, so the rod's image
/// clears quantization by an order of magnitude rather than sitting on it.
/// type-audit: bare-ok(ratio)
const SCOTOPIC_GAIN: f64 = 1000.0;

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
    // rgb indexes CHANNELS (long=2 -> red, medium=1 -> green, short=0 ->
    // blue); norms is indexed by OUTPUT SLOT (slot 0 = red = LONG_NORM).
    // These two orderings are easy to cross, and doing so is the most
    // likely bug in a change here — the_standard_observers_bytes_have_not_moved
    // is what catches it.
    let projection = Projection::new(
        "native",
        "the observer's own channels, carried straight to the screen",
        [2, 1, 0],
        [LONG_NORM, MEDIUM_NORM, SHORT_NORM],
    )
    .expect("the native projection's norms are nonzero finite constants");
    Observer::with_roles(
        vec![short, medium, long, scotopic],
        vec![
            ChannelRole::Chromatic,
            ChannelRole::Chromatic,
            ChannelRole::Chromatic,
            ChannelRole::Achromatic,
        ],
        Some(projection),
    )
    .expect("the standard observer's own construction is always valid")
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
    fn a_four_channel_synthetic_observer_still_has_no_srgb_image() {
        // Isolates the *no-projection* half of `to_srgb`'s guard. Its
        // sibling test uses a FIVE-channel observer, so the
        // `len() != self.channels.len()` check answers there and the
        // projection is never exercised — the assertion passes by the wrong
        // path, and stays green even if every observer carries a
        // projection. (Verified by mutation: giving `Observer::new` a
        // native projection reddens this test and nothing else.)
        //
        // Four channels is not a contrived count. It is exactly what
        // campaign 2 will build — a deuteranope is the standard observer
        // with a shifted medium channel, same shape, same arity. If the
        // flag ever stopped being checked, that observer would silently
        // borrow the human normalizers and render confidently wrong
        // colours, which is the RENDER-9 failure this guard exists to
        // prevent.
        let mut channels = Vec::new();
        for b in 0..4 {
            let mut curve = [0.0; BANDS];
            curve[b] = 1.0;
            channels.push(Spectrum::new(curve).unwrap());
        }
        let synthetic = Observer::new(channels).unwrap();
        assert_eq!(
            synthetic.channels(),
            4,
            "the arity must match the standard's"
        );
        let signal = synthetic.sense(&Reflectance::new([0.5; BANDS]).unwrap(), &flat_light());
        assert_eq!(signal.get().len(), 4, "so len() cannot be what refuses it");
        assert!(
            synthetic.to_srgb(&signal).is_none(),
            "a non-standard observer must have no sRGB image regardless of arity"
        );
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
        let p = obs.projection().expect("the standard observer projects");
        // Rounded to two places: the projection's norms are what to_srgb
        // uses, and a curve edit that does not update them would make a
        // white surface stop projecting to white. `sums` is indexed by
        // CHANNEL (0=short, 1=medium, 2=long); `p.norms` is indexed by
        // OUTPUT SLOT (0=red=long, 1=green=medium, 2=blue=short) — the two
        // orderings are deliberately crossed here.
        assert_eq!((sums[0] * 100.0).round() / 100.0, p.norms[2]);
        assert_eq!((sums[1] * 100.0).round() / 100.0, p.norms[1]);
        assert_eq!((sums[2] * 100.0).round() / 100.0, p.norms[0]);
        // The rod's normalizer is carried the same way and for the same
        // reason, but it is NOT on the projection: no projection may name an
        // achromatic channel. It is the fourth channel's sum.
        assert_eq!((sums[3] * 100.0).round() / 100.0, SCOTOPIC_NORM);
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

    #[test]
    fn a_projection_may_not_name_an_achromatic_channel() {
        // The whole point of roles: a rod carries no hue, so no projection may
        // read one. Without this, `observer_for` could silently build an eye
        // that shows brightness as blue.
        let curves = vec![
            Spectrum::new([0.5; BANDS]).unwrap(),
            Spectrum::new([0.5; BANDS]).unwrap(),
            Spectrum::new([0.5; BANDS]).unwrap(),
        ];
        let roles = vec![
            ChannelRole::Chromatic,
            ChannelRole::Chromatic,
            ChannelRole::Achromatic,
        ];
        // Index 2 is the achromatic channel.
        let p = Projection::new("bad", "nothing", [2, 1, 0], [1.0, 1.0, 1.0]).unwrap();
        let err = Observer::with_roles(curves, roles, Some(p)).unwrap_err();
        assert_eq!(err.unit, "observer");
    }

    #[test]
    fn a_projection_may_not_index_past_the_channel_set() {
        let curves = vec![Spectrum::new([0.5; BANDS]).unwrap()];
        let roles = vec![ChannelRole::Chromatic];
        let p = Projection::new("bad", "nothing", [0, 0, 7], [1.0, 1.0, 1.0]).unwrap();
        assert!(Observer::with_roles(curves, roles, Some(p)).is_err());
    }

    #[test]
    fn an_observer_needs_a_role_per_channel_and_one_chromatic_channel() {
        let one = || Spectrum::new([0.5; BANDS]).unwrap();
        // Mismatched lengths.
        assert!(
            Observer::with_roles(vec![one(), one()], vec![ChannelRole::Chromatic], None).is_err()
        );
        // An eye that carries no hue at all is not an eye this model can use.
        assert!(Observer::with_roles(vec![one()], vec![ChannelRole::Achromatic], None).is_err());
    }

    #[test]
    fn chromaticity_ignores_the_achromatic_channel() {
        // THE POINT OF THE WHOLE TASK, and the spec's M3. Two observers whose
        // chromatic channels are identical and whose achromatic channel differs
        // wildly must report the SAME chromaticity. Before roles existed, the
        // rod carried hue information and every dichromat measured as a
        // trichromat.
        let short = Spectrum::new([1.0, 1.0, 1.0, 0.5, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]).unwrap();
        let long = Spectrum::new([0.0, 0.0, 0.0, 0.0, 0.5, 1.0, 1.0, 1.0, 0.0, 0.0]).unwrap();
        let quiet_rod = Spectrum::new([0.0; BANDS]).unwrap();
        let loud_rod = Spectrum::new([1.0; BANDS]).unwrap();
        let roles = vec![
            ChannelRole::Chromatic,
            ChannelRole::Chromatic,
            ChannelRole::Achromatic,
        ];
        let a = Observer::with_roles(vec![short, long, quiet_rod], roles.clone(), None).unwrap();
        let b = Observer::with_roles(vec![short, long, loud_rod], roles, None).unwrap();
        let r = Reflectance::new([0.4; BANDS]).unwrap();
        let light = Illuminant::new([1.0; BANDS]).unwrap();
        let ca = a.chromaticity(&a.sense(&r, &light));
        let cb = b.chromaticity(&b.sense(&r, &light));
        assert_eq!(
            ca.len(),
            2,
            "chromaticity has one entry per CHROMATIC channel"
        );
        assert_eq!(ca, cb, "a louder rod must not move the chromaticity");
    }

    /// Simpson's rule is exact for cubics, so integrating a cubic over one band
    /// must return its analytic mean. This checks the QUADRATURE, independent of
    /// Planck — a Planck-only test cannot tell a broken rule from a broken law.
    ///
    /// FIRES WHEN: the weights, the node spacing, or the final division is wrong.
    #[test]
    fn the_band_quadrature_is_exact_for_a_cubic() {
        // mean of x^3 over [c - w/2, c + w/2] = c^3 + c * w^2 / 4
        let c = 500.0;
        let w = BAND_WIDTH_NM;
        let got = band_mean(c, &|x: f64| x * x * x);
        let want = c * c * c + c * w * w / 4.0;
        assert!(
            (got - want).abs() / want < 1e-12,
            "cubic band mean: got {got}, want {want}"
        );
    }

    /// The node count is a PERMANENT CONTRACT: change it and every colour in the
    /// world moves. It was chosen by measurement (spec §5.2) to hold at least
    /// 20x below a u8 quantization step (3.9e-3) down to 700 K, so that a later
    /// ember or forge cannot force it to change.
    ///
    /// FIRES WHEN: someone lowers BAND_NODES. Five nodes fails by 900 K.
    #[test]
    fn the_node_count_is_converged_down_to_a_dull_red_glow() {
        for t in [700.0, 1100.0, 1900.0, 5800.0] {
            for center in BAND_CENTERS_NM {
                let coarse = band_mean(center, &|nm| planck_relative(nm, t));
                let fine = band_mean_with_nodes(center, 4097, &|nm| planck_relative(nm, t));
                let rel = (coarse - fine).abs() / fine;
                assert!(
                    rel < 3.9e-4,
                    "T={t} band {center}: relative error {rel} exceeds 20x below a u8 step"
                );
            }
        }
    }

    /// A blackbody is peak-normalized, finite and positive — the contract every
    /// consumer relies on to compare COLOUR rather than distance from a source.
    ///
    /// FIRES WHEN: normalization is dropped or a band goes non-positive.
    #[test]
    fn a_blackbody_is_peak_normalized_and_positive() {
        let light = blackbody(1900.0);
        let peak = light.get().iter().copied().fold(0.0f64, f64::max);
        assert_eq!(peak, 1.0, "peak band should be exactly 1.0");
        for (b, v) in light.get().iter().enumerate() {
            assert!(v.is_finite() && *v > 0.0, "band {b} is {v}");
        }
    }

    #[test]
    fn the_standard_observers_bytes_have_not_moved() {
        // Two assertions below guard two DIFFERENT things; neither
        // subsumes the other.
        //
        // The `norms()` assertion is bit-exact and catches CARRIED-vs-
        // DERIVED: `Projection` carries its normalizers rather than
        // deriving them, because the shipped constants are the ROUNDED
        // channel sums — deriving live would move every colour the
        // standard observer has ever emitted. A live-derived version
        // differs from the carried constants by about 1 ULP on this
        // model's curves, and `to_srgb`'s `u8` rounding at this mid-grey
        // fixture ABSORBS that ULP — so only a comparison taken *before*
        // quantization to a byte can catch a derive-instead-of-carry
        // regression. (Confirmed by mutation below.)
        //
        // The `to_srgb` byte assertion is a *different* pin: the rgb/norms
        // crossed-ordering guard (channel index vs. output slot). The
        // norms assertion alone cannot see that bug, because it never
        // touches `rgb`.
        let obs = standard_observer();
        let light = Illuminant::new([1.0; BANDS]).unwrap();
        let mid = obs.sense(&Reflectance::new([0.5; BANDS]).unwrap(), &light);
        let p = obs.projection().expect("the standard observer projects");
        assert_eq!(p.name(), "native");
        assert_eq!(
            p.norms(),
            &[LONG_NORM, MEDIUM_NORM, SHORT_NORM],
            "norms must be the CARRIED constants, not a live-derived channel sum"
        );
        assert_eq!(obs.to_srgb(&mid).unwrap(), [188, 188, 188]);
    }

    // ---------------------------------------------------------------------
    // The scotopic term (The Lantern, spec §4.4; §6 H3 and H4).
    // ---------------------------------------------------------------------

    /// A pale limestone wall — the canonical cave surface, and the same
    /// authored reflectance the spec's own probe used
    /// (`windows/worldgen/tests/lantern_probe.rs`).
    ///
    /// **Deliberately not spectrally flat.** A flat reflectance under a flat
    /// illuminant drives all three output slots to the same value on its own,
    /// so it would satisfy [`the_rod_can_never_shift_a_hue`] without the rod
    /// doing anything at all — the "spectrally flat probe cancels the effect
    /// it measures" failure The Beholding shipped. Under [`torchlight`] this
    /// surface's blue slot is about a tenth of its red one, so a grey result
    /// can only have come from the rod.
    const LIMESTONE: [f64; BANDS] = [0.55, 0.68, 0.76, 0.80, 0.82, 0.83, 0.84, 0.84, 0.85, 0.85];

    /// The wall.
    fn limestone() -> Reflectance {
        Reflectance::new(LIMESTONE).expect("an authored reflectance is within [0, 1]")
    }

    /// A torch's own 1900 K light, scaled to `level` of its peak-normalized
    /// brightness. `level` is the *illuminance*, stated out loud by every
    /// caller, because every claim below is a claim about a light level.
    fn torchlight(level: f64) -> Illuminant {
        let base = blackbody(1900.0);
        let mut bands = [0.0f64; BANDS];
        for (out, value) in bands.iter_mut().zip(base.get()) {
            *out = value * level;
        }
        Illuminant::new(bands).expect("scaling a valid illuminant by a positive factor is valid")
    }

    /// The human-calibrated eye.
    fn human() -> Observer {
        standard_observer()
    }

    /// A rod-dominant eye, built here to the exact recipe
    /// `hornvale_worldgen::observer::observer_for` gives **kobold**
    /// (`night_vision` 0.9): hue depth 2, so medium and long merge
    /// `t = (0.9 - 0.5) / 0.5 = 0.8` of the way, and the rod carries the
    /// luminance-3 scotopic gain of 1.5.
    ///
    /// The kernel knows no species, so this cannot be imported — it is a
    /// transcription, and `windows/worldgen`'s own tests pin the recipe. What
    /// matters here is only the shape: a dichromat whose rod is 1.5x a
    /// human's.
    fn kobold() -> Observer {
        let merge = 0.8;
        let short = [0.00, 0.25, 1.00, 0.62, 0.10, 0.01, 0.00, 0.00, 0.00, 0.00];
        let medium = [0.00, 0.01, 0.10, 0.45, 0.90, 1.00, 0.72, 0.28, 0.05, 0.00];
        let long = [0.00, 0.01, 0.06, 0.25, 0.60, 0.92, 1.00, 0.75, 0.30, 0.06];
        let scotopic = [0.00, 0.15, 0.55, 0.95, 1.00, 0.68, 0.25, 0.05, 0.00, 0.00];
        let mut merged = [0.0; BANDS];
        let mut rod = [0.0; BANDS];
        for b in 0..BANDS {
            let mean = (medium[b] + long[b]) / 2.0;
            merged[b] = (1.0 - merge) * long[b] + merge * mean;
            rod[b] = scotopic[b] * 1.5;
        }
        let channels = vec![
            Spectrum::new(short).unwrap(),
            Spectrum::new(merged).unwrap(),
            Spectrum::new(rod).unwrap(),
        ];
        let roles = vec![
            ChannelRole::Chromatic,
            ChannelRole::Chromatic,
            ChannelRole::Achromatic,
        ];
        // Norms derived live from these exact curves, which is what
        // `observer_for` does for every non-human row.
        let unprojected = Observer::with_roles(channels.clone(), roles.clone(), None).unwrap();
        let white = unprojected.sense(
            &Reflectance::new([1.0; BANDS]).unwrap(),
            &Illuminant::new([1.0; BANDS]).unwrap(),
        );
        let rgb = [1usize, 1, 0];
        let norms = [white.get()[1], white.get()[1], white.get()[0]];
        let projection = Projection::new(
            "yellow-blue",
            "the short-to-long opposition; the red-green axis is not carried",
            rgb,
            norms,
        )
        .unwrap();
        Observer::with_roles(channels, roles, Some(projection)).unwrap()
    }

    /// The screen triple `observer` makes of a limestone wall under a torch
    /// dimmed to `level` — the last value in the chain, never an
    /// intermediate.
    fn swatch(observer: &Observer, level: f64) -> [u8; 3] {
        observer
            .to_srgb(&observer.sense(&limestone(), &torchlight(level)))
            .expect("both fixture eyes declare a projection")
    }

    /// The screen triple the CONES alone make — the projection exactly as it
    /// behaved before the scotopic term existed. The reference every H3
    /// assertion below compares against.
    fn photopic_only(observer: &Observer, level: f64) -> [u8; 3] {
        let signal = observer.sense(&limestone(), &torchlight(level));
        let slots = observer
            .photopic_slots(&signal)
            .expect("both fixture eyes declare a projection");
        [
            encode_srgb_byte(slots[0]),
            encode_srgb_byte(slots[1]),
            encode_srgb_byte(slots[2]),
        ]
    }

    /// Every triple below was captured from this file at `a5503433`, BEFORE
    /// the scotopic term existed: `(illuminance, human, kobold)`.
    ///
    /// The ladder runs from full torchlight down to `1e-3`, which is the
    /// dimmest light either eye can still put a byte on the screen under —
    /// one rung lower and the term takes over. Two rungs are there for
    /// reasons rather than for spacing: `5.88e-2` is `1 / (1 + 4²)`, the
    /// attenuation a chamber wall sees at the far edge of the sight radius,
    /// and `3e-3` renders `[2, 2, 0]`, which is exactly the dimmest cell the
    /// shipped game was measured to emit (the H4a reading in
    /// `windows/vessel/tests/lantern_night.rs`). The pin therefore brackets
    /// the whole range the chamber band actually occupies.
    const PRE_SCOTOPIC_LADDER: [(f64, [u8; 3], [u8; 3]); 7] = [
        (1e0, [137, 111, 40], [128, 128, 40]),
        (5e-1, [99, 80, 26], [92, 92, 26]),
        (1e-1, [44, 34, 7], [40, 40, 7]),
        (5.882_35e-2, [32, 24, 4], [30, 30, 4]),
        (1e-2, [8, 5, 1], [7, 7, 1]),
        (3e-3, [2, 2, 0], [2, 2, 0]),
        (1e-3, [1, 1, 0], [1, 1, 0]),
    ];

    /// H3 — the scotopic term is EXACTLY zero in daylight, so every colour
    /// The Beholding emits is unchanged. A REQUIREMENT with a test, not a
    /// prediction. Byte equality is exact equality: an epsilon assertion
    /// would pass through the drift it exists to catch, and a `u8` compare
    /// cannot be given one.
    ///
    /// FIRES WHEN: the blend leaks above the photopic threshold — which is
    /// what raising `PHOTOPIC_THRESHOLD` does, and is Task 7's step-5
    /// mutation.
    #[test]
    fn h3_the_scotopic_term_is_exactly_zero_in_daylight() {
        for (level, human_rgb, kobold_rgb) in PRE_SCOTOPIC_LADDER {
            assert_eq!(
                swatch(&human(), level),
                human_rgb,
                "the human's colour moved at illuminance {level:e}"
            );
            assert_eq!(
                swatch(&kobold(), level),
                kobold_rgb,
                "the kobold's colour moved at illuminance {level:e}"
            );
        }
    }

    /// H3 as a PROPERTY rather than a table: the term may only fill in a
    /// pixel the cones had already left black. Wherever the photopic image
    /// carries a single count in any slot, the emitted triple must be that
    /// image, unchanged.
    ///
    /// The table above pins seven levels; this sweeps sixty, three decades
    /// either side of the threshold, on both eyes — so a threshold moved by a
    /// factor of two cannot slip between two rungs of the ladder.
    ///
    /// FIRES WHEN: `PHOTOPIC_THRESHOLD` rises far enough that a visible pixel
    /// enters the blend.
    #[test]
    fn the_scotopic_term_never_touches_a_pixel_the_cones_could_reach() {
        let mut checked = 0;
        let mut level = 1.0;
        for _ in 0..60 {
            for eye in [human(), kobold()] {
                let cones = photopic_only(&eye, level);
                if cones != [0, 0, 0] {
                    assert_eq!(
                        swatch(&eye, level),
                        cones,
                        "illuminance {level:e}: the scotopic term altered a pixel the \
                         cones could already reach"
                    );
                    checked += 1;
                }
            }
            level *= 0.7;
        }
        assert!(
            checked > 20,
            "only {checked} of the sweep's levels produced a visible photopic pixel, \
             so this test barely measured H3 at all"
        );
    }

    /// The rod carries NO hue. Contributing equally to R, G and B, it can
    /// never shift one — so where the photopic signal is gone the three slots
    /// must be equal.
    ///
    /// **Stated at an illuminance whose result clears quantization by an
    /// order of magnitude** (byte 29 and byte 38, not byte 1): a scotopic
    /// contribution smaller than one `u8` step is invisible, and this test
    /// would then pass because *nothing at all* reached the screen rather
    /// than because the rod reached it evenly. The `>= 20` floor is what
    /// makes the equality mean something.
    ///
    /// The fixture is limestone under a 1900 K torch, which is emphatically
    /// **not** spectrally flat — the cones alone would put the blue slot at a
    /// tenth of the red one. A flat probe would make the three slots equal on
    /// its own and cancel the very effect this checks.
    ///
    /// FIRES WHEN: the rod is wired into one slot, or weighted per channel,
    /// or the photopic image is passed through underneath it.
    #[test]
    fn the_rod_can_never_shift_a_hue() {
        // Deep enough that the cones show nothing, bright enough that the rod
        // shows a great deal.
        let level = 2.5e-4;
        for (name, eye) in [("human", human()), ("kobold", kobold())] {
            assert_eq!(
                photopic_only(&eye, level),
                [0, 0, 0],
                "{name}: the cones still reach the screen at {level:e}, so this \
                 fixture is not in the scotopic regime at all"
            );
            let [r, g, b] = swatch(&eye, level);
            assert_eq!(
                (r, g),
                (g, b),
                "{name}: a rod-only signal produced a hue: [{r}, {g}, {b}]"
            );
            assert!(
                r >= 20,
                "{name}: the rod put only byte {r} on the screen — at that size the \
                 equality above holds because nothing arrived, not because the rod \
                 arrived evenly"
            );
        }
    }

    /// H4 — stated at the MODEL level deliberately (spec §6). Below the
    /// threshold a human emits `[0, 0, 0]` where a kobold does not.
    ///
    /// FIRES WHEN: night vision still cannot reach the screen — the measured
    /// pre-campaign state, where both eyes went black together while their
    /// rod signals differed (0.0003 vs 0.0004).
    ///
    /// **The gap is exactly one byte, and that is structural, not a weak
    /// fixture.** Both eyes carry the same scotopic curve; the kobold's is
    /// scaled by its 1.5 gain and nothing else, so in the fully dark-adapted
    /// regime the kobold's linear output is exactly 1.5x the human's at every
    /// illuminance. At the illuminance where the human's rounds below half a
    /// count, the kobold's is therefore at most three quarters of one — there
    /// is no illuminance at which the human is black and the kobold is bright.
    /// The claim here is the existence of the cliff. Its *height* is
    /// [`the_rod_can_never_shift_a_hue`]'s job, where both eyes are well up
    /// the scale and the kobold leads 38 to 29.
    #[test]
    fn h4_a_rod_dominant_eye_sees_where_a_human_does_not() {
        let level = 1.6e-6;
        let human_rgb = swatch(&human(), level);
        let kobold_rgb = swatch(&kobold(), level);
        assert_eq!(
            human_rgb,
            [0, 0, 0],
            "the human is not actually in the dark at {level:e}"
        );
        assert_ne!(
            kobold_rgb,
            [0, 0, 0],
            "H4 FALSIFIED: the kobold is blind too at {level:e}"
        );
        // Grey, not a hue: proof the kobold's pixel came from the rod path
        // rather than from a cone channel that happened to survive.
        assert_eq!(
            [kobold_rgb[0]; 3], kobold_rgb,
            "the kobold's pixel is not grey, so it did not come from the rod"
        );
    }
}
