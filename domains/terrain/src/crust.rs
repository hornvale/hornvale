//! Crust: drawn cratons and the stateless crust fields (Crust spec §2).
//! Everything here is a pure function of position — no per-cell draws —
//! so any grid at any level samples the same underlying world and
//! coarse-constrains-fine is exact.

use crate::pins::TerrainPins;
use crate::plates::dot;
use crate::rift::RiftHistory;
use crate::streams;
use hornvale_kernel::{Seed, math, noise};

/// Upper bound of `CrustKm`'s validated range, km. One named bound, two
/// consumers: `CrustKm::new`'s validation and `thickness_at`'s terrane
/// saturation cap (stacked terrane kernels must never push a validated
/// construction past this ceiling into a panic).
const CRUST_KM_MAX: f64 = 100.0;

/// Continental crust thickness in kilometers.
/// type-audit: newtype
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct CrustKm(f64);

impl CrustKm {
    /// Validate and wrap a thickness: finite, within [0, `CRUST_KM_MAX`] km.
    /// type-audit: bare-ok(constructor-edge)
    pub fn new(km: f64) -> Result<CrustKm, String> {
        if !km.is_finite() || !(0.0..=CRUST_KM_MAX).contains(&km) {
            return Err(format!("crust thickness {km} km outside [0, 100]"));
        }
        Ok(CrustKm(km))
    }

    /// The thickness in kilometers.
    /// type-audit: bare-ok(constructor-edge)
    pub fn get(&self) -> f64 {
        self.0
    }
}

/// Base spatial frequency of the lobing noise (features ~1/4 rad; Task 9
/// iteration 3': lowered from 6.0 so lobes are fewer and larger — smaller,
/// higher-frequency lobes were pinching craton margins into detached rim
/// fragments).
#[allow(dead_code)]
const LOBE_FREQ: f64 = 4.0;
/// fBm octaves for the lobing noise.
#[allow(dead_code)]
const LOBE_OCTAVES: u32 = 4;
/// Lobe amplitude: the rim radius varies in [1 - AMP, 1 + AMP] x radius,
/// i.e. [0.5, 1.5] x radius_rad at this value.
#[allow(dead_code)]
const LOBE_AMP: f64 = 0.5;

/// Contrast gain compensating the three-slice averaging in
/// `sphere_fbm01` (which compresses variance toward 0.5): the raw
/// average rarely reaches the extremes of [0, 1], so the centered
/// value is amplified before it drives the rim. The subsequent
/// `0.5 * (GAIN * (n - 0.5)).tanh()` (Task 9 iteration 2: a smooth
/// saturating map replacing the former hard `.clamp(-0.5, 0.5)`) keeps
/// the rim provably inside the *open* interval
/// (1 - LOBE_AMP, 1 + LOBE_AMP) x radius_rad — i.e. strictly inside
/// (0.5, 1.5) x radius_rad — regardless of gain, by tanh saturation:
/// tanh is bounded in (-1, 1) for every finite input, so the centered
/// value stays strictly inside (-0.5, 0.5) and the rim can approach
/// but never reach the bound. The hard clamp could jam the rim at
/// exactly 0.5x radius over whole plateaus of the noise field,
/// pinching craton margins into detached fragments; the smooth map
/// removes those plateaus. Increasing this constant only saturates
/// more of the input range earlier — it can never push the rim to or
/// past the bound.
///
/// Calibrated against 2000 seeds over the rim-spread test's exact ring
/// geometry (radius 0.3, angle 0.3, 64 azimuths): the smallest gain
/// giving >= 95% of seeds a rim-spread > 0.2 under the tanh map is
/// 15.0 (95.40%; 13 -> 93.15%, 14 -> 94.50%, 16 -> 95.95%). The tanh
/// map needs a higher gain than the clamp's 6.0 because it saturates
/// only asymptotically where the clamp cut off exactly; see the Task 9
/// iteration-2 report for the full sweep table.
#[allow(dead_code)]
const REBALANCE_GAIN: f64 = 15.0;

/// Precomputed seam-free spherical fBm: derives the three slice seeds and
/// their per-octave seeds **once**, then samples many positions with no
/// further derivation. Bit-identical to [`sphere_fbm01`] — same slice
/// labels, same slice-plane pairing, same summation order — it only hoists
/// the per-call `Seed::derive`s out of the per-cell loop. Build one per
/// field (the seed/frequency/octaves are loop-invariant) and call
/// [`SphereFbm::sample`] per cell.
#[derive(Clone, Debug)]
pub(crate) struct SphereFbm {
    /// The three orthogonal coordinate-plane slice samplers, in the
    /// `slice-0`/`slice-1`/`slice-2` order `sphere_fbm01` derives them.
    slices: [noise::Fbm; 3],
    /// Spatial frequency, applied to each coordinate at sample time.
    frequency: f64,
}

impl SphereFbm {
    /// Precompute the slice samplers for `octaves` octaves at `frequency`,
    /// rooted at `seed` exactly as `sphere_fbm01` does.
    pub(crate) fn new(seed: Seed, frequency: f64, octaves: u32) -> Self {
        Self {
            slices: [
                noise::Fbm::new(seed.derive("slice-0"), octaves),
                noise::Fbm::new(seed.derive("slice-1"), octaves),
                noise::Fbm::new(seed.derive("slice-2"), octaves),
            ],
            frequency,
        }
    }

    /// Sample the mean of the three orthogonal slices at position `p`.
    pub(crate) fn sample(&self, p: [f64; 3]) -> f64 {
        let f = self.frequency;
        (self.slices[0].sample(f * p[0], f * p[1])
            + self.slices[1].sample(f * p[1], f * p[2])
            + self.slices[2].sample(f * p[2], f * p[0]))
            / 3.0
    }
}

/// Seam-free fBm in [0, 1) on the unit sphere: the mean of three
/// orthogonal coordinate-plane slices (the `coast_render` construction).
///
/// Random-access convenience form; a hot per-cell loop with a fixed seed
/// should build a [`SphereFbm`] once and reuse it.
#[allow(dead_code)]
pub(crate) fn sphere_fbm01(seed: Seed, p: [f64; 3], frequency: f64, octaves: u32) -> f64 {
    SphereFbm::new(seed, frequency, octaves).sample(p)
}

/// Lobed envelope of one craton: 1 at the center, tapering to 0 at a rim
/// whose radius varies with direction (the fBm lobing), hard 0 beyond
/// 1.5x the nominal radius. Flat-topped quartic taper.
#[allow(dead_code)]
pub(crate) fn lobed_envelope(seed: Seed, center: [f64; 3], p: [f64; 3], radius_rad: f64) -> f64 {
    let angle = math::acos(dot(center, p).clamp(-1.0, 1.0));
    if angle >= 1.5 * radius_rad {
        return 0.0;
    }
    let n = sphere_fbm01(seed, p, LOBE_FREQ, LOBE_OCTAVES);
    let centered = 0.5 * math::tanh(REBALANCE_GAIN * (n - 0.5));
    let rim = radius_rad * (1.0 + 2.0 * LOBE_AMP * centered);
    let x = (angle / rim).clamp(0.0, 1.0);
    (1.0 - x * x).powi(2)
}

/// Thin oceanic floor thickness, km.
/// type-audit: pending(wave-2)
pub const OCEANIC_KM: f64 = 7.0;
/// Crust at or above this thickness is continental, km.
/// type-audit: pending(wave-2)
pub const CONTINENTAL_THRESHOLD_KM: f64 = 20.0;
/// Drawn craton peak thickness range, km. Minimum must clear
/// `elevation::ISOSTASY_REF_KM` (30 km) — otherwise an "old" craton
/// (age -> 1, peak -> `PEAK_MIN_KM`) floats at or below sea level and never
/// surfaces regardless of footprint area (Task 8's diagnosed finding, fixed
/// here in Task 9). At 33 km an old craton crests
/// `ISOSTASY_M_PER_KM * (33 - 30)` = ~540 m — comfortably above sea level.
const PEAK_MIN_KM: f64 = 33.0;
/// Upper end of the drawn peak range, km.
const PEAK_MAX_KM: f64 = 45.0;

/// A craton: a drawn nucleus of continental crust (Crust spec §2).
/// type-audit: bare-ok(index: id), bare-ok(ratio: center), pending(wave-2: radius_rad), bare-ok(ratio: age)
#[derive(Debug, Clone, PartialEq)]
pub struct Craton {
    /// Index into the drawn craton list.
    pub id: u32,
    /// Center on the unit sphere.
    pub center: [f64; 3],
    /// Nominal angular radius, radians; the lobed rim varies around it.
    pub radius_rad: f64,
    /// Age in [0, 1]: 0 young, 1 ancient (worn, low, quiet).
    pub age: f64,
}

/// An accreted exotic sliver welded to a continental leading margin
/// (Sculpting, spec §3): an anisotropic (elongated) crust kernel.
/// type-audit: bare-ok(ratio: center), bare-ok(ratio: along), pending(wave-2: half_len_rad), pending(wave-2: half_wid_rad), bare-ok(ratio: age), waiver(crust-km-convention: thickness_km)
#[derive(Debug, Clone, PartialEq)]
pub struct Terrane {
    /// Kernel center, a unit vector on the craton's rim.
    pub center: [f64; 3],
    /// Unit tangent along the margin (the elongation direction).
    pub along: [f64; 3],
    /// Angular half-length along the margin, radians.
    pub half_len_rad: f64,
    /// Angular half-width across the margin, radians.
    pub half_wid_rad: f64,
    /// Terrane age in [0,1] — young exotic crust, distinct from its host.
    pub age: f64,
    /// Peak added thickness, km.
    pub thickness_km: f64,
}

/// Terrane count lower bound (global knob, spec §5).
/// type-audit: bare-ok(count)
pub const TERRANE_COUNT_MIN: u32 = 2;
/// Terrane count upper bound.
/// type-audit: bare-ok(count)
pub const TERRANE_COUNT_MAX: u32 = 6;
/// Terrane angular half-length range, radians (~6°–14°).
/// type-audit: pending(wave-2)
pub const TERRANE_HALF_LEN_RAD: (f64, f64) = (0.10, 0.24);
/// Width is this fraction of length (elongation).
/// type-audit: bare-ok(ratio)
pub const TERRANE_ASPECT: f64 = 0.35;
/// Peak added thickness range, km.
/// type-audit: waiver(crust-km-convention)
pub const TERRANE_THICKNESS_KM: (f64, f64) = (6.0, 12.0);

/// Two unit tangent vectors orthogonal to `p` (and to each other),
/// forming an orthonormal frame for the tangent plane at `p` on the unit
/// sphere. Picks an arbitrary reference axis not nearly parallel to `p`
/// to keep the first cross product well-conditioned.
pub(crate) fn tangent_basis(p: [f64; 3]) -> ([f64; 3], [f64; 3]) {
    let reference = if p[0].abs() < 0.9 {
        [1.0, 0.0, 0.0]
    } else {
        [0.0, 1.0, 0.0]
    };
    let e1 = crate::plates::normalize(crate::plates::cross(p, reference));
    let e2 = crate::plates::normalize(crate::plates::cross(p, e1));
    (e1, e2)
}

/// Fixed-size scan used by `find_margin_point`: not a stream draw, so
/// the terrane draw count and order stay independent of the craton
/// field's noise (pin isolation).
const MARGIN_SEARCH_SAMPLES: u32 = 200;

/// Search outward from `host.center` along tangent direction `dir` (a
/// unit vector orthogonal to `host.center`) for the point whose lobed
/// envelope best matches the craton's own continental-threshold envelope
/// (`continental_threshold_envelope`) — i.e. the point that actually sits
/// on the margin. A nominal step of exactly `host.radius_rad` is not
/// reliable on its own: the lobed rim (`lobed_envelope`) wanders between
/// 0.5x and 1.5x that nominal radius depending on direction, so a fixed
/// step can land deep in open ocean (rim shrunk below the step) or deep
/// in the continental interior (rim expanded well past it). This is a
/// fixed-size numeric scan over the (deterministic, noise-based, drawless)
/// envelope function along the ray — no stream draws, so it perturbs
/// neither the terrane draw count nor its order.
fn find_margin_point(lobing_seed: Seed, host: &Craton, dir: [f64; 3]) -> [f64; 3] {
    let peak = PEAK_MIN_KM + (PEAK_MAX_KM - PEAK_MIN_KM) * (1.0 - host.age);
    let target = continental_threshold_envelope(peak);
    let mut best_point = host.center;
    let mut best_diff = f64::INFINITY;
    for i in 1..=MARGIN_SEARCH_SAMPLES {
        // Sweep just short of the hard 1.5x cutoff in `lobed_envelope`.
        let t = host.radius_rad * 1.45 * (i as f64) / (MARGIN_SEARCH_SAMPLES as f64);
        let p = crate::plates::rotate_toward(host.center, dir, t);
        let envelope = lobed_envelope(lobing_seed, host.center, p, host.radius_rad);
        let diff = (envelope - target).abs();
        if diff < best_diff {
            best_diff = diff;
            best_point = p;
        }
    }
    best_point
}

/// Draw the terrane set from the `terranes` stream: count, then per
/// terrane (host craton index, rim bearing, length, thickness, age),
/// sequentially. Placement: on the host craton's continental margin, at
/// a drawn bearing — drawn bearings are never rejected (no draw-count
/// variance, pin-isolation discipline), so the "leading margin" framing
/// is a doc description of the typical outcome (leading edges carry more
/// arc, hence more rim to land on), not an enforced constraint.
pub fn draw_terranes(
    terrain_seed: Seed,
    cratons: &[Craton],
    plates: &[crate::plates::Plate],
) -> Vec<Terrane> {
    let _ = plates;
    if cratons.is_empty() {
        return Vec::new();
    }
    let lobing_root = terrain_seed.derive(streams::CRATONS).derive("lobing");
    let mut stream = terrain_seed.derive(streams::TERRANES).stream();
    let count = stream.range_u32(TERRANE_COUNT_MIN, TERRANE_COUNT_MAX);
    (0..count)
        .map(|_| {
            let host = &cratons[stream.range_u32(0, cratons.len() as u32 - 1) as usize];
            let bearing = stream.next_f64() * core::f64::consts::TAU;
            let half_len_rad = TERRANE_HALF_LEN_RAD.0
                + (TERRANE_HALF_LEN_RAD.1 - TERRANE_HALF_LEN_RAD.0) * stream.next_f64();
            let thickness_km = TERRANE_THICKNESS_KM.0
                + (TERRANE_THICKNESS_KM.1 - TERRANE_THICKNESS_KM.0) * stream.next_f64();
            let age = 0.05 + 0.25 * stream.next_f64();
            // Radial direction: rotate an arbitrary tangent at the craton
            // center by `bearing`.
            let (e1, e2) = tangent_basis(host.center);
            let dir = [
                e1[0] * math::cos(bearing) + e2[0] * math::sin(bearing),
                e1[1] * math::cos(bearing) + e2[1] * math::sin(bearing),
                e1[2] * math::cos(bearing) + e2[2] * math::sin(bearing),
            ];
            // Margin point: search along `dir` for where this host's
            // actual (lobed) rim crosses the continental threshold —
            // not a fixed step, since the lobed rim itself varies with
            // direction (see `find_margin_point`'s doc).
            let lobing_seed = lobing_root.derive(&format!("craton-{}", host.id));
            let center = find_margin_point(lobing_seed, host, dir);
            // Margin tangent: perpendicular to the radial direction at `center`.
            let radial = dir;
            let along = crate::plates::normalize(crate::plates::cross(center, radial));
            Terrane {
                center,
                along,
                half_len_rad,
                half_wid_rad: half_len_rad * TERRANE_ASPECT,
                age,
                thickness_km,
            }
        })
        .collect()
}

/// Fixed microcontinent candidate draw count (survivors are <= this — the
/// away-from-majors filter below can only shrink the set, never grow it).
/// type-audit: bare-ok(count)
pub const MICRO_COUNT_MAX: u32 = 4;
/// Microcontinent radius range, radians (~1.5°–3°: Madagascar-scale at L6,
/// below the continent-count metric's 0.5%-of-land floor).
/// type-audit: pending(wave-2)
pub const MICRO_RADIUS_RAD: (f64, f64) = (0.026, 0.052);

/// Draw microcontinents: exactly `MICRO_COUNT_MAX` candidates — position
/// (`unit_vector`, two draws), radius, and age, three draws' worth each —
/// always drawn in full, in id order, regardless of which candidates
/// survive the filter below (fixed draw count; no rejection loop, so the
/// `MICROCONTINENTS` stream's total consumption never varies with the
/// major-craton layout it is filtered against — pin-isolation discipline,
/// the same shape `draw_terranes` uses for its own never-rejected bearing
/// draws).
///
/// A candidate survives only if every major craton in `cratons` sits
/// farther than `major.radius_rad + 2 * candidate.radius_rad` away —
/// generously outside both lobed rims' plausible overlap, so a
/// microcontinent never reads as fused onto a continent.
///
/// Ids continue the major sequence, assigned by *surviving* position
/// (`cratons.len()`, `cratons.len() + 1`, ...) rather than by
/// pre-filter candidate index: `CrustField` indexes its per-craton lobing
/// seed as `lobing_seeds[c.id as usize]`, which is only correct when a
/// craton's `id` equals its position in whatever list `CrustField` was
/// built over — true for majors (`draw_cratons` assigns `id: i` by
/// vector position) and preserved here only if survivors are
/// renumbered contiguously post-filter; a pre-filter id would leave gaps
/// whenever a candidate is discarded, breaking that invariant once this
/// set is concatenated onto the majors for `CrustField::new_with_terranes`.
///
/// Ages are drawn in `[0.3, 0.8]`, not the majors' full `[0, 1]`: paired
/// with the peak-thickness formula `thickness_at` shares with majors
/// (`PEAK_MIN_KM..=PEAK_MAX_KM` from `1 - age`), this keeps every
/// microcontinent's peak in `[35.4, 41.4]` km — comfortably above
/// `CONTINENTAL_THRESHOLD_KM` (20 km) at its center, so a small
/// oceanic-basin craton still reads as continental at its core. `Craton`
/// carries no separate thickness field (unlike `Terrane`) — thickness is
/// always this age-derived peak, for majors and microcontinents alike.
pub fn draw_microcontinents(terrain_seed: Seed, cratons: &[Craton]) -> Vec<Craton> {
    let mut stream = terrain_seed.derive(streams::MICROCONTINENTS).stream();
    let next_id = cratons.len() as u32;
    (0..MICRO_COUNT_MAX)
        .map(|_| {
            let center = crate::plates::unit_vector(&mut stream);
            let radius_rad =
                MICRO_RADIUS_RAD.0 + (MICRO_RADIUS_RAD.1 - MICRO_RADIUS_RAD.0) * stream.next_f64();
            let age = 0.3 + 0.5 * stream.next_f64();
            (center, radius_rad, age)
        })
        .filter(|(center, radius_rad, _)| {
            cratons.iter().all(|c| {
                math::acos(dot(*center, c.center).clamp(-1.0, 1.0))
                    > c.radius_rad + 2.0 * radius_rad
            })
        })
        .enumerate()
        .map(|(i, (center, radius_rad, age))| Craton {
            id: next_id + i as u32,
            center,
            radius_rad,
            age,
        })
        .collect()
}

/// A terrane's added thickness at `p`, km: a separable Gaussian in the
/// (along, across) tangent frame — elongated along the margin.
fn terrane_contribution_km(t: &Terrane, p: [f64; 3]) -> f64 {
    let angle = math::acos(crate::plates::dot(t.center, p).clamp(-1.0, 1.0));
    if angle > 4.0 * t.half_len_rad {
        return 0.0; // beyond support
    }
    // At the center itself the tangent-plane offset direction is
    // undefined (the radial projection is the zero vector, which
    // `normalize` cannot divide by) — but along/across are both exactly
    // 0 there by construction, so the contribution is the bare peak.
    if angle < 1e-12 {
        return t.thickness_km;
    }
    // Decompose the offset into along/across components in the tangent plane.
    let offset = crate::plates::normalize(crate::plates::sub(
        p,
        crate::plates::scale(t.center, crate::plates::dot(t.center, p)),
    ));
    let along_c = crate::plates::dot(offset, t.along) * angle;
    let across_c = (angle * angle - along_c * along_c).max(0.0).sqrt();
    t.thickness_km
        * math::exp(-(along_c * along_c) / (2.0 * t.half_len_rad * t.half_len_rad))
        * math::exp(-(across_c * across_c) / (2.0 * t.half_wid_rad * t.half_wid_rad))
}

/// A terrane's own envelope at `p`, in [0, 1]: its contribution
/// normalized by its own peak thickness, directly comparable to a
/// craton's `lobed_envelope` for the `age_at` winner comparison.
fn terrane_envelope(t: &Terrane, p: [f64; 3]) -> f64 {
    if t.thickness_km <= 0.0 {
        return 0.0;
    }
    terrane_contribution_km(t, p) / t.thickness_km
}

/// Spherical interpolation from `a` toward `b` by `t` in [0, 1]: the
/// great-circle analogue of a linear blend, so a slid center stays on the
/// unit sphere exactly rather than needing a post-hoc renormalize-to-taste.
fn slerp(a: [f64; 3], b: [f64; 3], t: f64) -> [f64; 3] {
    let omega = math::acos(crate::plates::dot(a, b).clamp(-1.0, 1.0));
    if omega < 1e-9 {
        return a;
    }
    // Near-antipodal (omega ≈ π) destabilizes 1/sin(omega) exactly as
    // near-zero destabilizes the great-circle direction itself — there the
    // arc is degenerate (a and b coincide); here it is degenerate the other
    // way (infinitely many great circles pass through antipodal points, so
    // no direction is privileged). Same fallback as the omega≈0 guard
    // above: leave `a` in place rather than divide by a near-zero sine.
    if omega > std::f64::consts::PI - 1e-9 {
        return a;
    }
    let (sa, sb) = (
        math::sin((1.0 - t) * omega) / math::sin(omega),
        math::sin(t * omega) / math::sin(omega),
    );
    crate::plates::normalize([
        sa * a[0] + sb * b[0],
        sa * a[1] + sb * b[1],
        sa * a[2] + sb * b[2],
    ])
}

/// Draw the craton set: budget (one draw, Task 9 iteration 3': now a
/// *margin* draw — see below), count (budget-scaled, no draw of its own),
/// then per-craton center (two), radius (one), age (one) — mirroring
/// `generate_plates`' pinned-count convention exactly: a pinned count
/// (`--continents`, Task 7) drives the per-craton loop directly, so a
/// pinned count larger than the drawn one consumes the same additional
/// draws the drawn path would have, and a pinned count smaller consumes
/// fewer — never skipping the count draws themselves (pin isolation).
/// `notes` receives a metering entry when `continents` is pinned (Task 7's
/// metering convention: `pinned <name> <value> (seed draws <drawn>)`).
///
/// Budget ← ocean fraction (Task 9 iteration 3'): the craton budget used to
/// be its own independently-drawn fraction of the sphere, matching the land
/// quota's *range* by construction but not its *value* — iteration 1 fixed
/// the range mismatch, but a seed whose drawn `ocean_fraction` came out
/// high could still land budget on the low end of its own separate range
/// (or vice versa), decoupled from what the percentile sea-level mechanism
/// would actually place. This iteration removes the decoupling: `budget =
/// (1.0 - ocean_target) * (1.0 + margin)`, where `ocean_target` is the
/// world's resolved ocean fraction (drawn or pinned — see
/// `elevation::resolve_ocean_fraction`, called once in `generate` and
/// threaded to both this function and `derive_sea_level`) and `margin =
/// 0.05 + 0.10 * u` with `u` the CRATONS stream's existing first draw,
/// reinterpreted rather than removed — so the label's draw *count* is
/// byte-unchanged, only what the draw *means* changes (a save-format
/// contract: see `streams::CRATONS`'s doc and `stream_labels()`). A pinned
/// `ocean_fraction` now legitimately conditions craton radii too — per the
/// pin doctrine, a pinned target conditions downstream identically to a
/// drawn one (see the pin-isolation test in `tectonic_properties.rs`).
/// Over the drawn range (`ocean_target` in \[0.5, 0.75), so land in
/// (0.25, 0.5\]; margin factor `1 + margin` in \[1.05, 1.15)), budget spans
/// roughly (0.2625, 0.575); `drawn_count = 3 + round(budget * 20)` spans
/// 8..=14 (recomputed honestly from those endpoints, replacing iteration
/// 1's 7..=13).
///
/// Continental-area normalization (Task 9 iteration 3', re-landing
/// iteration 3's derivation after its package was reverted for an
/// unrelated knob's calibration failure): the rescale below no longer
/// matches the *nominal cap area* to budget — it matches the cap's
/// *continental* area (the sub-region actually crossing
/// `CONTINENTAL_THRESHOLD_KM`), which is smaller than the full cap because
/// the lobed taper (`lobed_envelope`) is a flat-topped quartic,
/// `envelope(x) = (1 - x^2)^2` for `x = angle / rim`. Solving
/// `envelope(x) >= e` for the per-craton continental-threshold fraction
/// `e_i` gives `x^2 <= 1 - sqrt(e_i)`, and since cap area scales as `x^2`
/// at these radii, the continental fraction of a cap is `1 - sqrt(e_i)`,
/// with `e_i = (CONTINENTAL_THRESHOLD_KM - OCEANIC_KM) / (peak_i -
/// OCEANIC_KM)` and `peak_i` the same age-derived peak `thickness_at`
/// uses. Rescale: `Σ cap_area(r_i) * (1 - sqrt(e_i)) = budget * 4π`, same
/// `s = sqrt(target / current)` shape as iteration 1's simpler area match,
/// same 0.6 rad cap, zero extra stream draws. Over the peak range \[33,
/// 45\] km, `e_i` ∈ \[0.342, 0.500\] and `1 - sqrt(e_i)` ∈ \[0.293,
/// 0.415\] — radii grow roughly 1.6x versus the plain-area match, since
/// only a fraction of each nominal cap is actually continental.
///
/// `--supercontinent` no longer transforms craton centers here (epoch v4,
/// rift-and-fit spec §4): it holds the world at its pre-breakup ASSEMBLY
/// frame, applied in `globe::generate` after the rift is drawn (each
/// major's center becomes `rift.assembly[i]`). The draw stage leaves the
/// pinned set exactly as the unpinned path draws it, so the pin still
/// perturbs no draw and no area normalization (pin isolation).
///
/// Repulsion (Task 9 iteration 2, retargeted in iteration 3') is applied
/// last, and only when the world is *not* pinned `--supercontinent`: under
/// that pin the drawn set feeds `assemble_cratons` at genesis, which pulls
/// every craton back to first contact regardless of how the raw draw
/// scattered them, so a repulsion pass first would only respread an input
/// the assembly immediately collapses — skipping it keeps the pinned draw
/// equal to its own unrepelled path. One deterministic pass, in id order,
/// zero stream draws,
/// separating overlap-merged craton centers so distinct cratons read as
/// distinct landmasses (see `repel_cratons`). Like the radius rescale,
/// this makes the final *center* of cratons 1.. a downstream arithmetic
/// consequence of the whole set (radii, count) rather than a raw stream
/// draw; the stream-consumption order is untouched.
/// type-audit: bare-ok(prose: notes), bare-ok(ratio: ocean_target)
pub fn draw_cratons(
    terrain_seed: Seed,
    pins: &TerrainPins,
    ocean_target: f64,
    notes: &mut Vec<String>,
) -> Vec<Craton> {
    let mut cratons = draw_cratons_unrepelled(terrain_seed, pins, ocean_target, notes);
    if pins.supercontinent != Some(true) {
        repel_cratons(&mut cratons);
    }
    cratons
}

/// The lobed-envelope value `e` at which crust thickness exactly crosses
/// `CONTINENTAL_THRESHOLD_KM`, given a craton's age-derived peak
/// thickness (see `draw_cratons`'s doc for the full derivation). Shared
/// by `continental_cap_fraction` (the area-normalization rescale) and
/// `find_margin_point` (terrane placement): both need the same envelope
/// value that marks "the margin" for a craton of this peak.
fn continental_threshold_envelope(peak_km: f64) -> f64 {
    (CONTINENTAL_THRESHOLD_KM - OCEANIC_KM) / (peak_km - OCEANIC_KM)
}

/// The continental fraction of a craton's nominal cap: `1 - sqrt(e)`,
/// where `e` is `continental_threshold_envelope(peak_km)`. `peak_km` is
/// the same age-derived peak `thickness_at` computes.
fn continental_cap_fraction(peak_km: f64) -> f64 {
    1.0 - continental_threshold_envelope(peak_km).sqrt()
}

/// Continental cap area of one craton, steradians: the spherical-cap
/// area `2π(1 − cos r)` times the fraction of that cap actually crossing
/// `CONTINENTAL_THRESHOLD_KM` (see `continental_cap_fraction`). One
/// arithmetic, two callers: the `draw_cratons` rescale sums it over
/// pre-rescale radii; `continental_supply` over the final set.
fn craton_continental_steradians(c: &Craton) -> f64 {
    let peak = PEAK_MIN_KM + (PEAK_MAX_KM - PEAK_MIN_KM) * (1.0 - c.age);
    std::f64::consts::TAU * (1.0 - math::cos(c.radius_rad)) * continental_cap_fraction(peak)
}

/// Analytic continental supply of a craton set: the fraction of the
/// sphere's area whose crust crosses `CONTINENTAL_THRESHOLD_KM`, summed
/// over the final (post-rescale, post-repulsion) radii. Cap overlaps are
/// not deducted — an upper estimate, consistent with the rescale's own
/// accounting. Grid-free and draw-free: a pure function of the drawn
/// set, so every grid level sees the same supply.
/// type-audit: bare-ok(ratio: return)
pub fn continental_supply(cratons: &[Craton]) -> f64 {
    cratons
        .iter()
        .map(craton_continental_steradians)
        .sum::<f64>()
        / (4.0 * std::f64::consts::PI)
}

/// Everything in `draw_cratons` except the final repulsion pass: the draws,
/// the area-normalization rescale, and the `--supercontinent` transform.
/// Split out so the repulsion test can measure the pre-pass geometry.
fn draw_cratons_unrepelled(
    terrain_seed: Seed,
    pins: &TerrainPins,
    ocean_target: f64,
    notes: &mut Vec<String>,
) -> Vec<Craton> {
    let mut stream = terrain_seed.derive(streams::CRATONS).stream();
    let u = stream.next_f64();
    let margin = 0.05 + 0.10 * u;
    let budget = (1.0 - ocean_target) * (1.0 + margin);
    let drawn_count = 3 + (budget * 20.0).round() as u32; // 8..=14 over the drawn budget range
    let count = pins.continents.unwrap_or(drawn_count);
    if let Some(n) = pins.continents {
        notes.push(format!("pinned continents {n} (seed draws {drawn_count})"));
    }
    let mut cratons: Vec<Craton> = (0..count)
        .map(|i| {
            let center = crate::plates::unit_vector(&mut stream);
            let radius_rad = 0.10 + 0.35 * stream.next_f64();
            let age = stream.next_f64();
            Craton {
                id: i,
                center,
                radius_rad,
                age,
            }
        })
        .collect();
    let continental_area: f64 = cratons.iter().map(craton_continental_steradians).sum();
    if continental_area > 0.0 {
        let scale = ((budget * 4.0 * std::f64::consts::PI) / continental_area).sqrt();
        for c in cratons.iter_mut() {
            c.radius_rad = (c.radius_rad * scale).min(0.6);
        }
    }
    // Epoch v4 (rift-and-fit, spec §4): `--supercontinent` no longer
    // transforms craton centers at the draw stage. The pin now holds the
    // world at its pre-breakup ASSEMBLY frame, applied in `globe::generate`
    // after the rift is drawn (each major's center is replaced by
    // `rift.assembly[i]`), so the drawn set here is left exactly as the
    // unpinned path draws it — the suture is a genesis-stage consequence of
    // `crust::assemble_cratons`, not a raw-draw transform.
    cratons
}

/// Separation target multiplier applied to `r_i + r_j` during the
/// craton-center repulsion pass (Task 9 iteration 3', up from iteration
/// 2's implicit 1.0x): pairs are pushed toward 1.2x their combined radii
/// rather than exact rim tangency, leaving a moat of open ocean the lobed
/// rims' overlapping skirts are less likely to bridge back together.
const REPEL_SEPARATION_FACTOR: f64 = 1.2;

/// One deterministic repulsion pass over craton centers (Task 9
/// iteration 2, retargeted in iteration 3'): for each craton i > 0 in id
/// order, if any earlier-id craton j sits closer than
/// `REPEL_SEPARATION_FACTOR * (r_i + r_j)` radians, the later craton is
/// slerped directly AWAY from the earlier one to exactly that separation —
/// `slerp(c_j, c_i, t)` with `t = separation / omega > 1` extrapolates
/// past `c_i` along the `c_j -> c_i` great circle, staying on the unit
/// sphere exactly. Within craton i, the earlier-j sweep repeats (bounded
/// at `REPEL_SWEEPS`) until no earlier craton is violated — a single naive
/// j-sweep lets the fix for a later j undo the fix for an earlier one;
/// repeating within i settles each craton against ALL its predecessors
/// before moving on. Still one pass over cratons, id order, zero stream
/// draws, fully deterministic.
///
/// Under iteration 3's continental-area normalization the drawn cap
/// *footprints* can total well over half the sphere's surface for
/// crowded draws (see `draw_cratons`'s doc), so a fixed moat target is
/// not always geometrically satisfiable — settling craton i can re-crowd
/// a pair among 0..i only via i itself, and chains may leave residual
/// sub-target pairs. The guarantee this pass makes is therefore
/// *reduction*, not *attainment*: the minimum pairwise separation across
/// the whole set never gets worse than it was before the pass (see the
/// `repulsion_reduces_crowding_without_new_draws` test). Coincident
/// centers (omega ~ 0) are left in place: no repulsion direction is
/// privileged there.
fn repel_cratons(cratons: &mut [Craton]) {
    /// Bound on the within-craton settle sweeps: convergence is typically
    /// 2-3 sweeps; the cap only guarantees termination.
    const REPEL_SWEEPS: u32 = 16;
    for i in 1..cratons.len() {
        for _sweep in 0..REPEL_SWEEPS {
            let mut moved = false;
            for j in 0..i {
                let (c_j, r_j) = (cratons[j].center, cratons[j].radius_rad);
                let (c_i, r_i) = (cratons[i].center, cratons[i].radius_rad);
                let separation = REPEL_SEPARATION_FACTOR * (r_i + r_j);
                let omega = math::acos(dot(c_i, c_j).clamp(-1.0, 1.0));
                if omega > 1e-9 && omega < separation - 1e-12 {
                    cratons[i].center = slerp(c_j, c_i, separation / omega);
                    moved = true;
                }
            }
            if !moved {
                break;
            }
        }
    }
}

/// Fraction of `r_i + r_j` at which two cratons read as sutured (spec
/// §3.1): less than 1 so lobed rim overlap still merges before craton
/// *centers* reach exact tangency. Tuned only if the Task-6 pinned-world
/// suture test demands it.
/// type-audit: bare-ok(ratio)
pub const CONTACT_FACTOR: f64 = 0.85;

/// Sweep cap for `assemble_cratons`'s outward-push branch (a craton that
/// starts already overlapping an earlier one): mirrors `repel_cratons`'s
/// `REPEL_SWEEPS` exactly — clearing the worst-violating earlier craton
/// can re-violate a different one, so the push repeats until clear or
/// this cap is spent. The guarantee at the cap is *reduction*, not
/// *attainment* — the same honesty `repel_cratons` documents.
const ASSEMBLY_PUSH_SWEEPS: u32 = 16;

/// Doubling-search cap for the outward push's extrapolation factor: the
/// factor doubles from 2 until the push clears every placed craton, or
/// this many doublings are spent, whichever comes first.
const ASSEMBLY_PUSH_DOUBLINGS: u32 = 32;

/// Bisection iteration count shared by the inward pull and the outward
/// push: 64 fixed iterations rather than a tolerance, so the settled
/// position is deterministic bit-for-bit (no platform-dependent
/// early-exit).
const ASSEMBLY_BISECTION_ITERS: u32 = 64;

/// Angular separation between two unit-sphere points, radians.
fn angular_separation(a: [f64; 3], b: [f64; 3]) -> f64 {
    math::acos(dot(a, b).clamp(-1.0, 1.0))
}

/// The contact-separation target between two cratons: `CONTACT_FACTOR`
/// times their combined nominal radii.
fn contact_separation(a: &Craton, b: &Craton) -> f64 {
    CONTACT_FACTOR * (a.radius_rad + b.radius_rad)
}

/// The assembly objective for craton `craton_i` at a candidate position
/// `pos`: the smallest slack `sep(pos, assembly[j]) - contact(i, j)`
/// over every already-placed craton `j`. Positive means clear of every
/// placed craton; zero or negative means touching or overlapping the
/// tightest one. A plain fold (not a sort) — `placed`/`assembly` stay
/// index-aligned and small (craton counts are single digits to low
/// teens), so this is cheap to re-evaluate at every bisection step.
fn assembly_slack(
    pos: [f64; 3],
    craton_i: &Craton,
    placed: &[Craton],
    assembly: &[[f64; 3]],
) -> f64 {
    (0..placed.len())
        .map(|j| angular_separation(pos, assembly[j]) - contact_separation(craton_i, &placed[j]))
        .fold(f64::INFINITY, f64::min)
}

/// Index into `placed`/`assembly` of the worst-violated (most negative
/// slack) placed craton against `pos`, with that slack value.
fn worst_violation(
    pos: [f64; 3],
    craton_i: &Craton,
    placed: &[Craton],
    assembly: &[[f64; 3]],
) -> (usize, f64) {
    (0..placed.len())
        .map(|j| {
            let slack =
                angular_separation(pos, assembly[j]) - contact_separation(craton_i, &placed[j]);
            (j, slack)
        })
        .fold(
            (0, f64::INFINITY),
            |acc, cur| if cur.1 < acc.1 { cur } else { acc },
        )
}

/// Bisect the inward pull arc `slerp(start, anchor, t)`, `t` in
/// `[0, 1]`, for the exact `t` at which `assembly_slack` first reaches
/// zero. The caller has already checked slack is positive at `t = 0`
/// (the craton starts clear); slack at `t = 1` is always negative
/// because the arc lands exactly on the anchor — craton 0's already-
/// placed center — giving a zero separation against a strictly positive
/// contact target, so a sign change always exists. Bisection maintains
/// the invariant `slack(lo) > 0`, `slack(hi) <= 0` at every step
/// regardless of whether the objective is monotonic between them, so by
/// continuity the final `hi` converges to a point where some placed
/// craton is touched (not deeply overlapped) to within float precision.
fn pull_to_contact(
    start: [f64; 3],
    anchor: [f64; 3],
    craton_i: &Craton,
    placed: &[Craton],
    assembly: &[[f64; 3]],
) -> [f64; 3] {
    let (mut lo, mut hi) = (0.0_f64, 1.0_f64);
    for _ in 0..ASSEMBLY_BISECTION_ITERS {
        let mid = 0.5 * (lo + hi);
        let slack = assembly_slack(slerp(start, anchor, mid), craton_i, placed, assembly);
        if slack > 0.0 {
            lo = mid;
        } else {
            hi = mid;
        }
    }
    slerp(start, anchor, hi)
}

/// Bisect the outward extrapolation `slerp(anchor, pos, t)`, `t > 1`
/// (the same extrapolation shape `repel_cratons` uses), for the
/// smallest `t` at which `craton_i` is clear of every placed craton —
/// not just `anchor`. Doubles a trial factor from 2 until clear (capped
/// at `ASSEMBLY_PUSH_DOUBLINGS` doublings), then bisects `[1, t_hi]` for
/// the exact crossing with the same fixed-iteration shape as
/// `pull_to_contact`. If the doubling search never clears (pathological
/// geometry), the bisection still runs and returns its best crossing —
/// honesty about reduction, not attainment, mirrors `repel_cratons`.
fn push_to_clear(
    anchor: [f64; 3],
    pos: [f64; 3],
    craton_i: &Craton,
    placed: &[Craton],
    assembly: &[[f64; 3]],
) -> [f64; 3] {
    let mut hi = 2.0_f64;
    for _ in 0..ASSEMBLY_PUSH_DOUBLINGS {
        let clear = assembly_slack(slerp(anchor, pos, hi), craton_i, placed, assembly) >= 0.0;
        if clear {
            break;
        }
        hi *= 2.0;
    }
    let (mut lo, mut hi_bound) = (1.0_f64, hi);
    for _ in 0..ASSEMBLY_BISECTION_ITERS {
        let mid = 0.5 * (lo + hi_bound);
        let slack = assembly_slack(slerp(anchor, pos, mid), craton_i, placed, assembly);
        if slack < 0.0 {
            lo = mid;
        } else {
            hi_bound = mid;
        }
    }
    slerp(anchor, pos, hi_bound)
}

/// Push craton `i` away from whichever placed craton it violates most,
/// repeating against the next worst violator until clear or
/// `ASSEMBLY_PUSH_SWEEPS` is spent. Coincident (or antipodal) centers —
/// where `slerp`'s own guards make the push direction undefined — are
/// left in place for that pairing step, exactly as `repel_cratons`
/// documents for omega ~ 0: no push direction is privileged there, so
/// the sweep stops rather than chase an arbitrary one.
fn push_clear_of_overlap(
    start: [f64; 3],
    craton_i: &Craton,
    placed: &[Craton],
    assembly: &[[f64; 3]],
) -> [f64; 3] {
    let mut pos = start;
    for _sweep in 0..ASSEMBLY_PUSH_SWEEPS {
        let (worst_j, slack) = worst_violation(pos, craton_i, placed, assembly);
        if slack >= 0.0 {
            break;
        }
        let violator = assembly[worst_j];
        let omega = angular_separation(violator, pos);
        if !(1e-9..=std::f64::consts::PI - 1e-9).contains(&omega) {
            break;
        }
        pos = push_to_clear(violator, pos, craton_i, placed, assembly);
    }
    pos
}

/// Derive the contact-configuration assembly frame: where each craton's
/// center sits once cratons `1..` are pulled toward craton 0 (the
/// anchor) and settled at first contact along the way — repulsion
/// (`repel_cratons`) run in reverse (spec §3.1). Draw-free and
/// deterministic: a pure function of the drawn craton set, consuming no
/// `Seed`/`Stream`, never itself serialized (the assembly is derived on
/// demand, not saved world state).
///
/// For craton `i` in id order `1..`, the pull arc is
/// `slerp(cratons[i].center, anchor, t)`, `t` in `[0, 1]`. If the craton
/// starts clear of every already-placed craton, it slides inward and
/// settles the instant it first touches one (`pull_to_contact`,
/// bisected exactly, not by tolerance). If it starts already
/// overlapping one or more placed cratons, it is pushed directly away
/// from the worst-violating one instead (`push_clear_of_overlap`), the
/// same extrapolation `repel_cratons` uses, repeated against whichever
/// craton is worst-violating next. If it starts exactly at zero slack,
/// it is placed as-is. Craton 0 is the anchor and is never moved, so a
/// single-craton input is the identity map.
/// type-audit: bare-ok(ratio: return)
pub fn assemble_cratons(cratons: &[Craton]) -> Vec<[f64; 3]> {
    if cratons.is_empty() {
        return Vec::new();
    }
    let anchor = cratons[0].center;
    let mut assembly: Vec<[f64; 3]> = Vec::with_capacity(cratons.len());
    assembly.push(anchor);
    for i in 1..cratons.len() {
        let placed = &cratons[..i];
        let start = cratons[i].center;
        let f0 = assembly_slack(start, &cratons[i], placed, &assembly);
        let settled = match f0.partial_cmp(&0.0) {
            Some(std::cmp::Ordering::Greater) => {
                pull_to_contact(start, anchor, &cratons[i], placed, &assembly)
            }
            Some(std::cmp::Ordering::Less) => {
                push_clear_of_overlap(start, &cratons[i], placed, &assembly)
            }
            _ => start,
        };
        assembly.push(settled);
    }
    assembly
}

/// The crust fields: stateless functions of position over the drawn
/// craton set (Crust spec §2). Pure — the `Field` contract.
pub struct CrustField {
    /// The drawn cratons.
    cratons: Vec<Craton>,
    /// Per-craton lobing-kernel seeds, indexed by craton id — the exact
    /// `craton-{id}` derivations `strongest` used to recompute on every
    /// sample. Precomputed once here so per-cell and per-pixel sampling
    /// (millions of calls at the canonical grid) allocates and hashes
    /// nothing per craton. Byte-identical to the per-sample derivation.
    lobing_seeds: Vec<Seed>,
    /// The drawn terrane set (empty when built via `new`).
    terranes: Vec<Terrane>,
    /// The drawn rift history (rift-and-fit, spec §3.3), or `None` for the
    /// v3 field shape (`new`/`new_with_terranes`) — the clip in `strongest`
    /// is gated behind this `Option` entirely, so the `None` path runs the
    /// exact v3 arithmetic, instruction for instruction.
    rift: Option<RiftHistory>,
    /// Per-major-craton assembly-to-final rotation, index-aligned with
    /// `cratons[0..major_count]` (the major/microcontinent id-position
    /// invariant `draw_microcontinents` documents). Precomputed once here
    /// — mirrors `lobing_seeds` — so `strongest` never re-derives a
    /// rotation per sample; empty when `rift` is `None`.
    craton_rotations: Vec<([f64; 3], f64)>,
    /// Per-major-craton indices into `rift.seams` of the seams touching
    /// that craton (`rift::seam_indices_for`), index-aligned with
    /// `craton_rotations`. Precomputed once — the same caching shape as
    /// `lobing_seeds` — so per-sample clipping scans only the craton's own
    /// seams; empty when `rift` is `None`.
    craton_seams: Vec<Vec<usize>>,
    /// Cratons with `id < major_count` are clip-eligible; microcontinents
    /// (appended after the majors) and terranes ride unclipped. Ignored
    /// entirely when `rift` is `None`.
    major_count: usize,
}

impl CrustField {
    /// Assemble the field over a drawn craton set, with no terranes.
    pub fn new(terrain_seed: Seed, cratons: Vec<Craton>) -> CrustField {
        CrustField::new_with_terranes(terrain_seed, cratons, Vec::new())
    }

    /// Assemble the field over a drawn craton set and terrane set.
    pub fn new_with_terranes(
        terrain_seed: Seed,
        cratons: Vec<Craton>,
        terranes: Vec<Terrane>,
    ) -> CrustField {
        CrustField::new_with_rift(terrain_seed, cratons, terranes, None, 0)
    }

    /// Assemble the field over a drawn craton set, terrane set, and
    /// (optionally) a rift history that clips each major craton's cap
    /// along its assembly-frame seam curves (rift-and-fit, spec §3.3).
    /// `major_count` bounds clip application to majors (`id < major_count`)
    /// — microcontinents and terranes are never clipped; its value is
    /// ignored entirely when `rift` is `None`.
    /// type-audit: bare-ok(count: major_count)
    pub fn new_with_rift(
        terrain_seed: Seed,
        cratons: Vec<Craton>,
        terranes: Vec<Terrane>,
        rift: Option<RiftHistory>,
        major_count: usize,
    ) -> CrustField {
        let lobing_root = terrain_seed.derive(streams::CRATONS).derive("lobing");
        let lobing_seeds = cratons
            .iter()
            .map(|c| lobing_root.derive(&format!("craton-{}", c.id)))
            .collect();
        let craton_rotations = match &rift {
            Some(r) => (0..major_count.min(cratons.len()))
                .map(|i| crate::rift::rotation_for(r.assembly[i], cratons[i].center))
                .collect(),
            None => Vec::new(),
        };
        let craton_seams = match &rift {
            Some(r) => (0..major_count.min(cratons.len()))
                .map(|i| crate::rift::seam_indices_for(r, cratons[i].id))
                .collect(),
            None => Vec::new(),
        };
        CrustField {
            cratons,
            lobing_seeds,
            terranes,
            rift,
            craton_rotations,
            craton_seams,
            major_count,
        }
    }

    /// Envelope and winning craton at a position. Two branches, not one
    /// with a no-op multiply: the `None` branch is byte-identical to v3's
    /// `strongest` (untouched instruction-for-instruction), since even a
    /// multiply-by-`1.0` clip factor is not guaranteed to be a float no-op
    /// in every case — the empty-rift byte-identity contract needs the old
    /// code path literally unperturbed, not merely numerically close.
    fn strongest(&self, p: [f64; 3]) -> Option<(f64, &Craton)> {
        match &self.rift {
            None => self
                .cratons
                .iter()
                .map(|c| {
                    let seed = self.lobing_seeds[c.id as usize];
                    (lobed_envelope(seed, c.center, p, c.radius_rad), c)
                })
                .max_by(|(a, ca), (b, cb)| a.total_cmp(b).then(cb.id.cmp(&ca.id))),
            Some(rift) => self
                .cratons
                .iter()
                .map(|c| {
                    let seed = self.lobing_seeds[c.id as usize];
                    let envelope = lobed_envelope(seed, c.center, p, c.radius_rad);
                    // Two byte-identity-preserving skips, not fidelity calls
                    // (the Task 6 perf budget's recovery — ~2.7x over ->
                    // within budget — with zero output bytes moved):
                    // (1) `envelope != 0.0`: the clip is a finite value in
                    //     [0, 1] and `0.0 * clip == 0.0` exactly, so gating
                    //     it behind a nonzero envelope is bit-equal
                    //     everywhere. `lobed_envelope` returns a hard 0.0
                    //     outside 1.5x radius, and at any sample only a
                    //     craton or two is nonzero, so the (seam-noise-
                    //     heavy) clip is paid per OVERLAPPING craton, not
                    //     per craton.
                    // (2) empty seam list: `clip_over_seams` returns exactly
                    //     1.0 for a seamless craton, and `envelope * 1.0`
                    //     is bit-equal to `envelope` (IEEE 754 multiply by
                    //     1.0 is exact), so the call is skipped outright.
                    // The seam subset itself is precomputed per major at
                    // construction (`craton_seams`) — same seams, same
                    // order, same min-fold as the full-scan filter.
                    let idx = c.id as usize;
                    let envelope = if envelope != 0.0
                        && idx < self.major_count
                        && !self.craton_seams[idx].is_empty()
                    {
                        let rotation = self.craton_rotations[idx];
                        let clip = crate::rift::clip_over_seams(
                            rift,
                            &self.cratons,
                            c.id,
                            rotation,
                            p,
                            &self.craton_seams[idx],
                        );
                        envelope * clip
                    } else {
                        envelope
                    };
                    (envelope, c)
                })
                .max_by(|(a, ca), (b, cb)| a.total_cmp(b).then(cb.id.cmp(&ca.id))),
        }
    }

    /// Crust thickness at a unit-sphere position, km: the winning
    /// craton's contribution plus every terrane's (kernels are additive
    /// summands, so thickness stays a pure pointwise function of `p`).
    /// type-audit: bare-ok(ratio: p)
    pub fn thickness_at(&self, p: [f64; 3]) -> CrustKm {
        let base = match self.strongest(p) {
            None => OCEANIC_KM,
            Some((envelope, c)) => {
                let peak = PEAK_MIN_KM + (PEAK_MAX_KM - PEAK_MIN_KM) * (1.0 - c.age);
                OCEANIC_KM + (peak - OCEANIC_KM) * envelope
            }
        };
        let added: f64 = self
            .terranes
            .iter()
            .map(|t| terrane_contribution_km(t, p))
            .sum();
        // Stacking terranes saturate rather than overflow (physically:
        // accreted crust delaminates past the isostatic limit) — up to
        // TERRANE_COUNT_MAX kernels can pile on one host (hosts are drawn
        // with replacement; --continents 1 forces it), and 45 + 6 * 12 km
        // would breach CrustKm's validated ceiling as a raw panic.
        CrustKm::new((base + added).min(CRUST_KM_MAX)).expect("kernel keeps thickness in range")
    }

    /// Age of the winning craton at a position (oceanic floor: young, 0),
    /// unless a terrane's own kernel dominates there — a terrane is
    /// younger, distinct exotic crust, so its age wins wherever its
    /// envelope is stronger than the winning craton's.
    /// type-audit: bare-ok(ratio)
    pub fn age_at(&self, p: [f64; 3]) -> f64 {
        let (craton_envelope, craton_age) = match self.strongest(p) {
            Some((envelope, c)) if envelope > 0.0 => (envelope, c.age),
            _ => (0.0, 0.0),
        };
        let terrane_winner = self
            .terranes
            .iter()
            .map(|t| (terrane_envelope(t, p), t.age))
            .max_by(|(a, _), (b, _)| a.total_cmp(b));
        match terrane_winner {
            Some((envelope, age)) if envelope > craton_envelope => age,
            _ => craton_age,
        }
    }

    /// Continental iff thickness clears the threshold.
    /// type-audit: bare-ok(ratio: p), bare-ok(flag: return)
    pub fn continental_at(&self, p: [f64; 3]) -> bool {
        self.thickness_at(p).get() >= CONTINENTAL_THRESHOLD_KM
    }
}

impl hornvale_kernel::Field<f64> for CrustField {
    /// Thickness in km; `Position.x` = longitude degrees, `Position.y` =
    /// latitude degrees (terrain's declared interpretation, spec §2);
    /// crust is static — time is ignored.
    fn sample(&self, pos: hornvale_kernel::Position, _time: hornvale_kernel::WorldTime) -> f64 {
        let (lat, lon) = (pos.y.to_radians(), pos.x.to_radians());
        let p = [
            math::cos(lat) * math::cos(lon),
            math::cos(lat) * math::sin(lon),
            math::sin(lat),
        ];
        self.thickness_at(p).get()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::streams;
    use hornvale_kernel::{Geosphere, NearestCellIndex};

    /// The default-pins ocean-fraction target for a given terrain seed —
    /// what `generate` resolves once and threads to both `draw_cratons`
    /// and `derive_sea_level` (Task 9 iteration 3'). Tests that exercise
    /// `draw_cratons` directly (outside `generate`) need this to supply a
    /// realistic budget.
    fn default_ocean_target(terrain_seed: Seed) -> f64 {
        crate::elevation::resolve_ocean_fraction(
            terrain_seed,
            &TerrainPins::default(),
            &mut Vec::new(),
        )
    }

    /// `n` roughly-evenly-spaced points on the unit sphere (the standard
    /// golden-angle fibonacci-sphere construction), for byte-identity and
    /// other whole-field sweeps that want broad coverage without a full
    /// `Geosphere` grid.
    fn fibonacci_sphere_points(n: usize) -> Vec<[f64; 3]> {
        let golden_angle = core::f64::consts::PI * (3.0 - 5.0_f64.sqrt());
        (0..n)
            .map(|i| {
                let y = 1.0 - 2.0 * (i as f64) / ((n - 1) as f64);
                let radius = (1.0 - y * y).max(0.0).sqrt();
                let theta = golden_angle * (i as f64);
                [math::cos(theta) * radius, y, math::sin(theta) * radius]
            })
            .collect()
    }

    #[test]
    fn crust_km_validates() {
        assert!(CrustKm::new(35.0).is_ok());
        assert!(CrustKm::new(-1.0).is_err());
        assert!(CrustKm::new(f64::NAN).is_err());
        assert!(CrustKm::new(101.0).is_err());
        assert_eq!(CrustKm::new(7.0).unwrap().get(), 7.0);
    }

    #[test]
    fn envelope_is_one_at_center_zero_far_and_monotone_on_average() {
        let seed = Seed(42).derive("test-craton");
        let center = [0.0, 0.0, 1.0];
        assert!((lobed_envelope(seed, center, center, 0.3) - 1.0).abs() < 1e-9);
        // Far beyond any possible lobed rim (rim <= 1.5 * radius).
        let far = [0.0, 0.0, -1.0];
        assert_eq!(lobed_envelope(seed, center, far, 0.3), 0.0);
        // Radially: average envelope over a ring shrinks as rings move out.
        let ring_mean = |angle: f64| -> f64 {
            (0..64)
                .map(|i| {
                    let az = std::f64::consts::TAU * (i as f64) / 64.0;
                    let s = math::sin(angle);
                    let p = [s * math::cos(az), s * math::sin(az), math::cos(angle)];
                    lobed_envelope(seed, center, p, 0.3)
                })
                .sum::<f64>()
                / 64.0
        };
        assert!(ring_mean(0.1) > ring_mean(0.25));
        assert!(ring_mean(0.25) > ring_mean(0.40));
    }

    #[test]
    fn envelope_rims_are_lobed_not_circular() {
        // At a fixed angular distance near the rim, envelope varies with
        // azimuth: some directions are inside a lobe, others outside.
        // Swept across seeds (not just one) so the assertion isn't riding
        // on a single lucky/unlucky draw.
        for seed_val in [1u64, 7, 42, 99] {
            let seed = Seed(seed_val).derive("test-craton");
            let center = [0.0, 0.0, 1.0];
            let values: Vec<f64> = (0..64)
                .map(|i| {
                    let az = std::f64::consts::TAU * (i as f64) / 64.0;
                    let s = math::sin(0.3f64);
                    let p = [s * math::cos(az), s * math::sin(az), math::cos(0.3f64)];
                    lobed_envelope(seed, center, p, 0.3)
                })
                .collect();
            let (min, max) = values
                .iter()
                .fold((f64::INFINITY, f64::NEG_INFINITY), |(lo, hi), v| {
                    (lo.min(*v), hi.max(*v))
                });
            assert!(
                max - min > 0.2,
                "rim too circular for seed {seed_val}: spread {}",
                max - min
            );
        }
    }

    #[test]
    fn craton_draws_are_sequential_and_in_range() {
        for seed in 0..16u64 {
            let terrain_seed = Seed(seed).derive(streams::ROOT);
            let ocean_target = default_ocean_target(terrain_seed);
            let cratons = draw_cratons(
                terrain_seed,
                &TerrainPins::default(),
                ocean_target,
                &mut Vec::new(),
            );
            assert!(
                (8..=14).contains(&cratons.len()),
                "seed {seed}: {}",
                cratons.len()
            );
            for (i, c) in cratons.iter().enumerate() {
                assert_eq!(c.id, i as u32);
                assert!((crate::plates::norm(c.center) - 1.0).abs() < 1e-12);
                // Post-rescale bound, exact by construction rather than
                // observed: scale = sqrt(budget * 4pi / continental_area) is
                // strictly positive (budget > 0, continental_area > 0), so
                // radius_rad = min(r * scale, 0.6) is always in (0, 0.6].
                // The pre-rescale draw range (0.10..=0.45) no longer bounds
                // it — that is the area-normalization's whole point.
                assert!(
                    c.radius_rad > 0.0 && c.radius_rad <= 0.6,
                    "radius {}",
                    c.radius_rad
                );
                assert!((0.0..=1.0).contains(&c.age));
            }
        }
    }

    #[test]
    fn repulsion_reduces_crowding_without_new_draws() {
        // Minimum pairwise angular separation over a craton set.
        let min_pairwise_angle = |cratons: &[Craton]| -> f64 {
            let mut min = f64::INFINITY;
            for i in 1..cratons.len() {
                for j in 0..i {
                    let angle =
                        math::acos(dot(cratons[i].center, cratons[j].center).clamp(-1.0, 1.0));
                    min = min.min(angle);
                }
            }
            min
        };
        for seed in 0..8u64 {
            let terrain_seed = Seed(seed).derive(streams::ROOT);
            let ocean_target = default_ocean_target(terrain_seed);
            let mut notes = Vec::new();
            let pre = draw_cratons_unrepelled(
                terrain_seed,
                &TerrainPins::default(),
                ocean_target,
                &mut notes,
            );
            let mut post = pre.clone();
            repel_cratons(&mut post);
            // The public path is exactly pre + one repulsion pass.
            assert_eq!(
                post,
                draw_cratons(
                    terrain_seed,
                    &TerrainPins::default(),
                    ocean_target,
                    &mut Vec::new()
                ),
                "seed {seed}: public path diverges from unrepelled + repel"
            );
            // (b) Zero extra draws: everything but centers is
            // byte-identical between pre- and post-repulsion.
            for (a, b) in pre.iter().zip(&post) {
                assert_eq!(a.id, b.id);
                assert_eq!(a.radius_rad, b.radius_rad);
                assert_eq!(a.age, b.age);
                assert!((crate::plates::norm(b.center) - 1.0).abs() < 1e-12);
            }
            // (a) Reduction, not attainment: under iteration 3's
            // continental-area normalization the drawn cap footprints can
            // total well over half the sphere's surface for crowded
            // draws, so a fixed 1.2x-of-target moat is not always
            // geometrically satisfiable (settling craton i can re-crowd a
            // pair among 0..i only via i itself). What the pass
            // guarantees instead: the minimum pairwise separation across
            // the whole set never gets worse than it was before the pass.
            assert!(
                min_pairwise_angle(&post) >= min_pairwise_angle(&pre) - 1e-12,
                "seed {seed}: repulsion reduced the minimum pairwise \
                 separation ({} -> {})",
                min_pairwise_angle(&pre),
                min_pairwise_angle(&post)
            );
        }
        // (c) Skipped under --supercontinent: the public path equals the
        // unrepelled path exactly (that pin wants clustering, not
        // separation).
        for seed in 0..8u64 {
            let terrain_seed = Seed(seed).derive(streams::ROOT);
            let ocean_target = default_ocean_target(terrain_seed);
            let pins = TerrainPins {
                supercontinent: Some(true),
                ..TerrainPins::default()
            };
            let unrepelled =
                draw_cratons_unrepelled(terrain_seed, &pins, ocean_target, &mut Vec::new());
            let public = draw_cratons(terrain_seed, &pins, ocean_target, &mut Vec::new());
            assert_eq!(
                unrepelled, public,
                "seed {seed}: repulsion ran despite --supercontinent"
            );
        }
    }

    #[test]
    fn assembly_is_identity_for_single_craton_and_sutures_multis() {
        let seed = Seed(42).derive(crate::streams::ROOT);
        let mut notes = Vec::new();
        let pins = TerrainPins::default();
        let ocean_target = default_ocean_target(seed);
        let cratons = draw_cratons(seed, &pins, ocean_target, &mut notes);
        let assembly = assemble_cratons(&cratons);
        assert_eq!(assembly.len(), cratons.len());
        // Every craton i > 0 touches at least one earlier craton at the
        // contact separation (within 1e-9 rad) and none sits closer than
        // contact to ANY earlier craton (no deep overlap).
        for i in 1..cratons.len() {
            let mut touched = false;
            for j in 0..i {
                let sep = math::acos(dot(assembly[i], assembly[j]).clamp(-1.0, 1.0));
                let contact = CONTACT_FACTOR * (cratons[i].radius_rad + cratons[j].radius_rad);
                assert!(sep >= contact - 1e-9, "craton {i} overlaps {j}");
                if sep <= contact + 1e-9 {
                    touched = true;
                }
            }
            assert!(touched, "craton {i} floats free of the assembly");
        }
        // Single craton: identity.
        let one = vec![cratons[0].clone()];
        assert_eq!(assemble_cratons(&one), vec![cratons[0].center]);
        // Determinism.
        assert_eq!(assembly, assemble_cratons(&cratons));
    }

    #[test]
    fn assembly_is_draw_free() {
        // `assemble_cratons` takes no `Seed`/`Stream` — its signature is
        // its own proof, but this documents the consequence: repeated
        // calls on independent clones of the same craton set must be
        // byte-identical, since there is nothing left to draw from.
        // Swept across seeds so the assertion isn't riding on one draw.
        for seed_val in [1u64, 7, 42] {
            let terrain_seed = Seed(seed_val).derive(streams::ROOT);
            let ocean_target = default_ocean_target(terrain_seed);
            let cratons = draw_cratons(
                terrain_seed,
                &TerrainPins::default(),
                ocean_target,
                &mut Vec::new(),
            );
            let a = assemble_cratons(&cratons.clone());
            let b = assemble_cratons(&cratons.clone());
            assert_eq!(a, b, "seed {seed_val}: assembly is not draw-free");
        }
    }

    #[test]
    fn continents_pin_overrides_without_perturbing_shared_cratons() {
        let seed = Seed(42).derive(streams::ROOT);
        let ocean_target = default_ocean_target(seed);
        let drawn = draw_cratons(seed, &TerrainPins::default(), ocean_target, &mut Vec::new());
        let pinned = draw_cratons(
            seed,
            &TerrainPins {
                continents: Some(5),
                ..TerrainPins::default()
            },
            ocean_target,
            &mut Vec::new(),
        );
        assert_eq!(pinned.len(), 5);
        assert_eq!(
            drawn[0].center, pinned[0].center,
            "craton 0 center perturbed"
        );
        for (a, b) in drawn.iter().zip(&pinned) {
            assert_eq!(a.id, b.id);
            assert_eq!(a.age, b.age, "shared-prefix craton {} age perturbed", a.id);
            // radius_rad and (for cratons 1..) center are deliberately NOT
            // asserted equal: the Task 9 area-normalization rescale
            // redistributes the same drawn budget across whatever count of
            // cratons survives the pin (fewer cratons means larger radii),
            // and the Task 9 iteration-2 repulsion pass then displaces
            // centers by amounts that depend on those radii and on which
            // cratons exist. Both are downstream arithmetic consequences
            // of the pin, not stream-draw perturbations — age (straight
            // from the stream) and craton 0's center (never repelled) are
            // the pin-isolation invariants this test protects; the raw
            // center draws' invariance is covered structurally by the
            // repulsion test's pre/post comparison.
        }
        // The pre-repulsion shared-prefix centers ARE stream-identical —
        // the raw draws are pin-isolated; only the post-transforms differ.
        let drawn_pre =
            draw_cratons_unrepelled(seed, &TerrainPins::default(), ocean_target, &mut Vec::new());
        let pinned_pre = draw_cratons_unrepelled(
            seed,
            &TerrainPins {
                continents: Some(5),
                ..TerrainPins::default()
            },
            ocean_target,
            &mut Vec::new(),
        );
        for (a, b) in drawn_pre.iter().zip(&pinned_pre) {
            assert_eq!(
                a.center, b.center,
                "shared-prefix craton {} raw center draw perturbed",
                a.id
            );
        }
    }

    #[test]
    fn supercontinent_no_longer_transforms_the_drawn_craton_set() {
        // Epoch v4 (rift-and-fit, spec §4): `--supercontinent` no longer
        // clusters cratons at the draw stage. Its only draw-stage effect is
        // the (pre-existing) repulsion skip; the suture is now a
        // genesis-stage consequence of `assemble_cratons` (see
        // `pinned_supercontinent_is_sutured` in tests/tectonic_properties.rs).
        let seed = Seed(42).derive(streams::ROOT);
        let ocean_target = default_ocean_target(seed);
        let scattered = draw_cratons(seed, &TerrainPins::default(), ocean_target, &mut Vec::new());
        let pinned = draw_cratons(
            seed,
            &TerrainPins {
                supercontinent: Some(true),
                ..TerrainPins::default()
            },
            ocean_target,
            &mut Vec::new(),
        );
        assert_eq!(scattered.len(), pinned.len());
        // Position-independent draws (radius, age) are byte-identical: the
        // pin perturbs no draw, and no longer moves any center toward
        // craton 0.
        for (s, p) in scattered.iter().zip(&pinned) {
            assert_eq!(s.id, p.id);
            assert_eq!(s.radius_rad, p.radius_rad);
            assert_eq!(s.age, p.age);
        }
        // The pinned path equals its own unrepelled draw exactly — the only
        // draw-stage difference from the scattered path is the repulsion
        // skip (repulsion would respread an input the genesis assembly
        // immediately collapses).
        let pinned_unrepelled = draw_cratons_unrepelled(
            seed,
            &TerrainPins {
                supercontinent: Some(true),
                ..TerrainPins::default()
            },
            ocean_target,
            &mut Vec::new(),
        );
        assert_eq!(pinned, pinned_unrepelled);
        // Some(false) re-affirms the scattered (repelled) layout byte-identically.
        let reaffirmed = draw_cratons(
            seed,
            &TerrainPins {
                supercontinent: Some(false),
                ..TerrainPins::default()
            },
            ocean_target,
            &mut Vec::new(),
        );
        assert_eq!(scattered, reaffirmed);
    }

    #[test]
    fn crust_field_is_pure_and_bounded() {
        let seed = Seed(42).derive(streams::ROOT);
        let ocean_target = default_ocean_target(seed);
        let field = CrustField::new(
            seed,
            draw_cratons(seed, &TerrainPins::default(), ocean_target, &mut Vec::new()),
        );
        let p = [0.6, -0.48, 0.64];
        let p = crate::plates::normalize(p);
        assert_eq!(field.thickness_at(p), field.thickness_at(p));
        let t = field.thickness_at(p).get();
        assert!((OCEANIC_KM..=60.0).contains(&t));
        assert!((0.0..=1.0).contains(&field.age_at(p)));
        assert_eq!(field.continental_at(p), t >= CONTINENTAL_THRESHOLD_KM);
    }

    #[test]
    fn crust_field_agrees_at_vertices_shared_across_levels() {
        // Icosphere levels nest: every level-4 vertex is a level-5 vertex.
        let seed = Seed(7).derive(streams::ROOT);
        let ocean_target = default_ocean_target(seed);
        let field = CrustField::new(
            seed,
            draw_cratons(seed, &TerrainPins::default(), ocean_target, &mut Vec::new()),
        );
        let coarse = Geosphere::new(4);
        let fine = Geosphere::new(5);
        let index = NearestCellIndex::new(&fine);
        let mut shared = 0;
        for cell in coarse.cells() {
            let p = coarse.position(cell);
            let c = coarse.coord(cell);
            let twin = index.nearest(&fine, c.latitude, c.longitude);
            let q = fine.position(twin);
            if crate::plates::dot(p, q) > 1.0 - 1e-12 {
                shared += 1;
                assert_eq!(
                    field.thickness_at(p).get(),
                    field.thickness_at(q).get(),
                    "field disagrees at a shared vertex"
                );
            }
        }
        assert!(
            shared > coarse.cell_count() / 2,
            "nesting assumption broken: {shared}"
        );
    }

    #[test]
    fn kernel_field_sampling_matches_direct_evaluation() {
        let seed = Seed(42).derive(streams::ROOT);
        let ocean_target = default_ocean_target(seed);
        let field = CrustField::new(
            seed,
            draw_cratons(seed, &TerrainPins::default(), ocean_target, &mut Vec::new()),
        );
        let pos = hornvale_kernel::Position {
            x: -120.25,
            y: 45.5,
        };
        let time = hornvale_kernel::WorldTime { day: 3.0 };
        let (lat, lon) = (45.5f64.to_radians(), (-120.25f64).to_radians());
        let p = [
            math::cos(lat) * math::cos(lon),
            math::cos(lat) * math::sin(lon),
            math::sin(lat),
        ];
        use hornvale_kernel::Field;
        assert_eq!(field.sample(pos, time), field.thickness_at(p).get());
    }

    #[test]
    fn terranes_are_drawn_on_continental_margins_and_thicken_crust() {
        let seed = Seed(42).derive(crate::streams::ROOT);
        let mut notes = Vec::new();
        let pins = crate::pins::TerrainPins::default();
        let plates = crate::plates::generate_plates(seed, &pins, &mut notes);
        let ocean_target = 0.65;
        let cratons = draw_cratons(seed, &pins, ocean_target, &mut notes);
        let terranes = draw_terranes(seed, &cratons, &plates);
        assert!(
            (2..=6).contains(&terranes.len()),
            "count: {}",
            terranes.len()
        );
        let plain = CrustField::new(seed, cratons.clone());
        let with = CrustField::new_with_terranes(seed, cratons, terranes.clone());
        for t in &terranes {
            // A terrane thickens crust at its own center...
            assert!(with.thickness_at(t.center).get() > plain.thickness_at(t.center).get());
            // ...and sits at a continental margin: near the continental
            // threshold in the plain field (rim, not deep interior/abyss).
            let base = plain.thickness_at(t.center).get();
            assert!(
                base > OCEANIC_KM && base < CONTINENTAL_THRESHOLD_KM + 12.0,
                "terrane not on a margin: base {base}"
            );
            // Elongated: at one half-length along `along`, still thickened;
            // at the same distance across, contribution has fallen off harder.
            assert!(t.half_len_rad > t.half_wid_rad);
        }
        // Determinism.
        let again = draw_terranes(
            Seed(42).derive(crate::streams::ROOT),
            &draw_cratons(
                Seed(42).derive(crate::streams::ROOT),
                &pins,
                ocean_target,
                &mut Vec::new(),
            ),
            &plates,
        );
        assert_eq!(terranes, again);
    }

    #[test]
    fn microcontinents_are_small_oceanic_and_deterministic() {
        let seed = Seed(7).derive(crate::streams::ROOT);
        let pins = crate::pins::TerrainPins::default();
        let mut notes = Vec::new();
        let ocean_target = default_ocean_target(seed);
        let cratons = draw_cratons(seed, &pins, ocean_target, &mut notes);
        let micro = draw_microcontinents(seed, &cratons);
        assert!(micro.len() <= MICRO_COUNT_MAX as usize);
        for m in &micro {
            assert!(m.radius_rad <= MICRO_RADIUS_RAD.1);
            // Far from every major craton: no merger into a continent.
            for c in &cratons {
                let d = math::acos(dot(m.center, c.center).clamp(-1.0, 1.0));
                assert!(
                    d > c.radius_rad + 2.0 * m.radius_rad,
                    "microcontinent fused to a craton"
                );
            }
        }
        assert_eq!(micro, draw_microcontinents(seed, &cratons));
        // Ids continue the major sequence contiguously (no gaps), so a
        // caller concatenating `[cratons, micro].concat()` gets
        // `id == position` throughout the combined list — the invariant
        // `CrustField`'s per-craton lobing-seed lookup depends on.
        for (i, m) in micro.iter().enumerate() {
            assert_eq!(m.id, cratons.len() as u32 + i as u32);
        }
    }

    #[test]
    fn stacked_terranes_saturate_at_the_crust_ceiling() {
        // Deterministic worst case: a young craton (peak 45 km) with the
        // maximum terrane count, all at maximum thickness, all centered
        // on the craton's own peak — 45 + 6 * 12 = 117 km unclamped. The
        // sum must saturate at CrustKm's validated ceiling, not panic.
        let seed = Seed(42).derive(crate::streams::ROOT);
        let center = [0.0, 0.0, 1.0];
        let craton = Craton {
            id: 0,
            center,
            radius_rad: 0.3,
            age: 0.0,
        };
        let stack: Vec<Terrane> = (0..TERRANE_COUNT_MAX)
            .map(|_| Terrane {
                center,
                along: [1.0, 0.0, 0.0],
                half_len_rad: TERRANE_HALF_LEN_RAD.1,
                half_wid_rad: TERRANE_HALF_LEN_RAD.1 * TERRANE_ASPECT,
                age: 0.1,
                thickness_km: TERRANE_THICKNESS_KM.1,
            })
            .collect();
        let field = CrustField::new_with_terranes(seed, vec![craton], stack);
        assert_eq!(field.thickness_at(center).get(), CRUST_KM_MAX);
    }

    #[test]
    fn continental_supply_is_the_area_the_rescale_budgets() {
        // Empty set: no supply.
        assert_eq!(continental_supply(&[]), 0.0);
        // A lone pinned craton clamps at 0.6 rad: supply is capped below the
        // 0.6 rad cap area (1 - cos 0.6)/2 ~= 8.73% of the sphere times the
        // best-case (young, peak 45 km) continental fraction ~0.415 ~= 3.63%.
        for seed in 1..=8u64 {
            let terrain_seed = Seed(seed).derive(streams::ROOT);
            let ocean_target = default_ocean_target(terrain_seed);
            let pins = TerrainPins {
                continents: Some(1),
                ..TerrainPins::default()
            };
            let cratons = draw_cratons(terrain_seed, &pins, ocean_target, &mut Vec::new());
            let supply = continental_supply(&cratons);
            assert!(
                supply > 0.0 && supply < 0.037,
                "seed {seed}: supply {supply}"
            );
        }
        // Default draws (8-14 cratons): supply sits near the land budget,
        // an order of magnitude above the single-craton ceiling.
        for seed in 0..16u64 {
            let terrain_seed = Seed(seed).derive(streams::ROOT);
            let ocean_target = default_ocean_target(terrain_seed);
            let cratons = draw_cratons(
                terrain_seed,
                &TerrainPins::default(),
                ocean_target,
                &mut Vec::new(),
            );
            let supply = continental_supply(&cratons);
            assert!(
                (0.15..=0.60).contains(&supply),
                "seed {seed}: supply {supply}"
            );
        }
    }

    #[test]
    fn empty_rift_is_byte_identical_to_v3_field() {
        let seed = Seed(42).derive(streams::ROOT);
        let pins = TerrainPins::default();
        let ocean_target = default_ocean_target(seed);
        let cratons = draw_cratons(seed, &pins, ocean_target, &mut Vec::new());
        let plates = crate::plates::generate_plates(seed, &pins, &mut Vec::new());
        let terranes = draw_terranes(seed, &cratons, &plates);
        let major_count = cratons.len();
        let v3 = CrustField::new_with_terranes(seed, cratons.clone(), terranes.clone());
        let with_none = CrustField::new_with_rift(seed, cratons, terranes, None, major_count);
        for p in fibonacci_sphere_points(200) {
            assert_eq!(
                v3.thickness_at(p),
                with_none.thickness_at(p),
                "empty-rift field diverged from v3 at {p:?}"
            );
            // `age_at` shares the same `strongest` winner comparison as
            // `thickness_at`, so the `None` rift path must leave it
            // byte-identical too (Task 5 review carry-over): a clip that
            // silently perturbed the winning craton would move ages even
            // where it left thicknesses alone.
            assert_eq!(
                v3.age_at(p),
                with_none.age_at(p),
                "empty-rift age_at diverged from v3 at {p:?}"
            );
        }
    }

    #[test]
    fn clip_flips_continental_at_across_a_synthetic_seam() {
        // Two hand-placed cratons ~0.5 rad apart, radii 0.3 — closer than
        // their contact separation (`CONTACT_FACTOR * 0.6 = 0.51 rad`), so
        // `assemble_cratons` nudges craton 1 OUTWARD by ~0.01 rad
        // (`push_clear_of_overlap`) to first contact: the final centers are
        // NOT the assembly centers, they differ by that small push, and
        // craton 1's clip therefore rides a real (if tiny) assembly->final
        // rotation rather than the identity. The clip still determines
        // whether a point near the shared boundary reads continental.
        let a = Craton {
            id: 0,
            center: [1.0, 0.0, 0.0],
            radius_rad: 0.3,
            age: 0.0,
        };
        let b_center = crate::plates::normalize([math::cos(0.5), math::sin(0.5), 0.0]);
        let b = Craton {
            id: 1,
            center: b_center,
            radius_rad: 0.3,
            age: 0.0,
        };
        let cratons = vec![a.clone(), b.clone()];
        let terrain_seed = Seed(1).derive(streams::ROOT);
        let rift = crate::rift::draw_rift(terrain_seed, &cratons);
        assert!(
            !rift.seams.is_empty(),
            "hand-placed cratons must contact and seam"
        );
        let field =
            CrustField::new_with_rift(terrain_seed, cratons.clone(), Vec::new(), Some(rift), 2);
        let plain = CrustField::new(terrain_seed, cratons);
        // Deep in craton a's interior: continental in both fields, clip near 1.
        assert!(field.continental_at(a.center));
        assert!(plain.continental_at(a.center));
        // Deep in craton b's interior, on its far side away from a (well
        // past the seam on b's own side): also continental in both — the
        // clip only cuts a craton's OWN side away from a seam it loses,
        // never the side it wins.
        let deep_in_b = crate::plates::normalize([math::cos(0.65), math::sin(0.65), 0.0]);
        assert!(field.continental_at(deep_in_b));
        assert!(plain.continental_at(deep_in_b));
        // Between the two centers, near the seam but still on what was
        // craton a's unclipped footprint: the plain (v3) field still
        // reads continental there (a's envelope alone clears the
        // threshold), but the clipped field does not — a's cap has been
        // cut back to the seam, so nothing continental remains at that
        // point once b's (near-zero, un-clipped-for-b) envelope also
        // fails to clear it. This is the field-level clip consequence
        // Task 6's wiring relies on.
        let mut flipped = false;
        for i in 0..200 {
            let t = (i as f64) / 199.0;
            let angle = 0.05 + 0.45 * t; // sweep between the two centers
            let p = crate::plates::normalize([math::cos(angle), math::sin(angle), 0.0]);
            if plain.continental_at(p) && !field.continental_at(p) {
                flipped = true;
                break;
            }
        }
        assert!(
            flipped,
            "clip never flipped continental_at from true (v3) to false (clipped) along the seam sweep"
        );
    }
}
