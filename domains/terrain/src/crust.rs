//! Crust: drawn cratons and the stateless crust fields (Crust spec §2).
//! Everything here is a pure function of position — no per-cell draws —
//! so any grid at any level samples the same underlying world and
//! coarse-constrains-fine is exact.

use crate::pins::TerrainPins;
use crate::plates::dot;
use crate::streams;
use hornvale_kernel::{Seed, noise};

/// Continental crust thickness in kilometers.
/// type-audit: newtype
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct CrustKm(f64);

impl CrustKm {
    /// Validate and wrap a thickness: finite, within [0, 100] km.
    /// type-audit: bare-ok(constructor-edge)
    pub fn new(km: f64) -> Result<CrustKm, String> {
        if !km.is_finite() || !(0.0..=100.0).contains(&km) {
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

/// Base spatial frequency of the lobing noise (features ~1/6 rad).
#[allow(dead_code)]
const LOBE_FREQ: f64 = 6.0;
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
/// `.clamp(-0.5, 0.5)` keeps the rim provably inside
/// [1 - LOBE_AMP, 1 + LOBE_AMP] x radius_rad (i.e. [0.5, 1.5] x
/// radius_rad) regardless of gain, by construction — increasing this
/// constant only spends more of that fixed range on smaller inputs,
/// it can never push the rim outside the bound.
///
/// Calibrated against 5000 seeds over the rim-spread test's exact ring
/// geometry (radius 0.3, angle 0.3, 64 azimuths): the smallest gain
/// giving >= 95% of seeds a rim-spread > 0.2 was 6.0 (95.1% pass
/// rate); see the Task 4 review fix report for the full sweep table.
#[allow(dead_code)]
const REBALANCE_GAIN: f64 = 6.0;

/// Seam-free fBm in [0, 1) on the unit sphere: the mean of three
/// orthogonal coordinate-plane slices (the `coast_render` construction).
#[allow(dead_code)]
pub(crate) fn sphere_fbm01(seed: Seed, p: [f64; 3], frequency: f64, octaves: u32) -> f64 {
    let slices = [
        (seed.derive("slice-0"), p[0], p[1]),
        (seed.derive("slice-1"), p[1], p[2]),
        (seed.derive("slice-2"), p[2], p[0]),
    ];
    slices
        .iter()
        .map(|(s, a, b)| noise::fbm_2d(*s, frequency * a, frequency * b, octaves))
        .sum::<f64>()
        / 3.0
}

/// Lobed envelope of one craton: 1 at the center, tapering to 0 at a rim
/// whose radius varies with direction (the fBm lobing), hard 0 beyond
/// 1.5x the nominal radius. Flat-topped quartic taper.
#[allow(dead_code)]
pub(crate) fn lobed_envelope(seed: Seed, center: [f64; 3], p: [f64; 3], radius_rad: f64) -> f64 {
    let angle = dot(center, p).clamp(-1.0, 1.0).acos();
    if angle >= 1.5 * radius_rad {
        return 0.0;
    }
    let n = sphere_fbm01(seed, p, LOBE_FREQ, LOBE_OCTAVES);
    let centered = ((n - 0.5) * REBALANCE_GAIN).clamp(-0.5, 0.5);
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
/// Drawn craton peak thickness range, km.
const PEAK_MIN_KM: f64 = 30.0;
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

/// How far each non-anchor craton slides toward craton 0 when
/// `--supercontinent` clusters them: 0 leaves it in place, 1 collapses it
/// onto the anchor exactly.
const CLUSTER_PULL: f64 = 0.75;

/// Spherical interpolation from `a` toward `b` by `t` in [0, 1]: the
/// great-circle analogue of a linear blend, so a slid center stays on the
/// unit sphere exactly rather than needing a post-hoc renormalize-to-taste.
fn slerp(a: [f64; 3], b: [f64; 3], t: f64) -> [f64; 3] {
    let omega = crate::plates::dot(a, b).clamp(-1.0, 1.0).acos();
    if omega < 1e-9 {
        return a;
    }
    let (sa, sb) = (
        ((1.0 - t) * omega).sin() / omega.sin(),
        (t * omega).sin() / omega.sin(),
    );
    crate::plates::normalize([
        sa * a[0] + sb * b[0],
        sa * a[1] + sb * b[1],
        sa * a[2] + sb * b[2],
    ])
}

/// Draw the craton set: budget (one draw), count (one draw, budget-scaled),
/// then per-craton center (two), radius (one), age (one) — mirroring
/// `generate_plates`' pinned-count convention exactly: a pinned count
/// (`--continents`, Task 7) drives the per-craton loop directly, so a
/// pinned count larger than the drawn one consumes the same additional
/// draws the drawn path would have, and a pinned count smaller consumes
/// fewer — never skipping the budget or count draws themselves (pin
/// isolation). `notes` receives a metering entry when `continents` is
/// pinned (Task 7's metering convention: `pinned <name> <value> (seed draws
/// <drawn>)`). `--supercontinent` is applied last, after every position is
/// drawn: it clusters cratons 1.. toward craton 0 by `slerp`, so the pin
/// perturbs positions but never the draw sequence (pin isolation again).
/// type-audit: bare-ok(prose: notes)
pub fn draw_cratons(
    terrain_seed: Seed,
    pins: &TerrainPins,
    notes: &mut Vec<String>,
) -> Vec<Craton> {
    let mut stream = terrain_seed.derive(streams::CRATONS).stream();
    let budget = 0.15 + 0.25 * stream.next_f64();
    let drawn_count = 3 + (budget * 20.0).round() as u32; // 6..=11 over the budget range
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
    if pins.supercontinent == Some(true) && cratons.len() > 1 {
        let anchor = cratons[0].center;
        for craton in cratons.iter_mut().skip(1) {
            craton.center = slerp(craton.center, anchor, CLUSTER_PULL);
        }
    }
    cratons
}

/// The crust fields: stateless functions of position over the drawn
/// craton set (Crust spec §2). Pure — the `Field` contract.
pub struct CrustField {
    /// Noise seed for the lobing kernels (per-craton derivations inside).
    seed: Seed,
    /// The drawn cratons.
    cratons: Vec<Craton>,
}

impl CrustField {
    /// Assemble the field over a drawn craton set.
    pub fn new(terrain_seed: Seed, cratons: Vec<Craton>) -> CrustField {
        CrustField {
            seed: terrain_seed.derive(streams::CRATONS).derive("lobing"),
            cratons,
        }
    }

    /// Envelope and winning craton at a position.
    fn strongest(&self, p: [f64; 3]) -> Option<(f64, &Craton)> {
        self.cratons
            .iter()
            .map(|c| {
                let seed = self.seed.derive(&format!("craton-{}", c.id));
                (lobed_envelope(seed, c.center, p, c.radius_rad), c)
            })
            .max_by(|(a, ca), (b, cb)| a.total_cmp(b).then(cb.id.cmp(&ca.id)))
    }

    /// Crust thickness at a unit-sphere position, km.
    /// type-audit: bare-ok(ratio: p)
    pub fn thickness_at(&self, p: [f64; 3]) -> CrustKm {
        let t = match self.strongest(p) {
            None => OCEANIC_KM,
            Some((envelope, c)) => {
                let peak = PEAK_MIN_KM + (PEAK_MAX_KM - PEAK_MIN_KM) * (1.0 - c.age);
                OCEANIC_KM + (peak - OCEANIC_KM) * envelope
            }
        };
        CrustKm::new(t).expect("kernel keeps thickness in range")
    }

    /// Age of the winning craton at a position (oceanic floor: young, 0).
    /// type-audit: bare-ok(ratio)
    pub fn age_at(&self, p: [f64; 3]) -> f64 {
        match self.strongest(p) {
            Some((envelope, c)) if envelope > 0.0 => c.age,
            _ => 0.0,
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
        let p = [lat.cos() * lon.cos(), lat.cos() * lon.sin(), lat.sin()];
        self.thickness_at(p).get()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::streams;
    use hornvale_kernel::{Geosphere, NearestCellIndex};

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
                    let s = angle.sin();
                    let p = [s * az.cos(), s * az.sin(), angle.cos()];
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
                    let s = 0.3f64.sin();
                    let p = [s * az.cos(), s * az.sin(), 0.3f64.cos()];
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
            let cratons = draw_cratons(
                Seed(seed).derive(streams::ROOT),
                &TerrainPins::default(),
                &mut Vec::new(),
            );
            assert!(
                (6..=11).contains(&cratons.len()),
                "seed {seed}: {}",
                cratons.len()
            );
            for (i, c) in cratons.iter().enumerate() {
                assert_eq!(c.id, i as u32);
                assert!((crate::plates::norm(c.center) - 1.0).abs() < 1e-12);
                assert!(
                    (0.10..=0.45).contains(&c.radius_rad),
                    "radius {}",
                    c.radius_rad
                );
                assert!((0.0..=1.0).contains(&c.age));
            }
        }
    }

    #[test]
    fn continents_pin_overrides_without_perturbing_shared_cratons() {
        let seed = Seed(42).derive(streams::ROOT);
        let drawn = draw_cratons(seed, &TerrainPins::default(), &mut Vec::new());
        let pinned = draw_cratons(
            seed,
            &TerrainPins {
                continents: Some(5),
                ..TerrainPins::default()
            },
            &mut Vec::new(),
        );
        assert_eq!(pinned.len(), 5);
        for (a, b) in drawn.iter().zip(&pinned) {
            assert_eq!(a, b, "shared-prefix craton {} perturbed", a.id);
        }
    }

    #[test]
    fn supercontinent_clusters_cratons_without_new_draws() {
        let seed = Seed(42).derive(streams::ROOT);
        let scattered = draw_cratons(seed, &TerrainPins::default(), &mut Vec::new());
        let clustered = draw_cratons(
            seed,
            &TerrainPins {
                supercontinent: Some(true),
                ..TerrainPins::default()
            },
            &mut Vec::new(),
        );
        assert_eq!(scattered.len(), clustered.len());
        let center = scattered[0].center;
        for (s, c) in scattered.iter().zip(&clustered) {
            assert_eq!(s.radius_rad, c.radius_rad);
            assert_eq!(s.age, c.age);
            if s.id == 0 {
                assert_eq!(s.center, c.center);
            } else {
                // Every clustered center is strictly closer to craton 0.
                assert!(
                    crate::plates::dot(c.center, center)
                        > crate::plates::dot(s.center, center) - 1e-12
                );
            }
        }
        // Some(false) re-affirms scattered byte-identically.
        let reaffirmed = draw_cratons(
            seed,
            &TerrainPins {
                supercontinent: Some(false),
                ..TerrainPins::default()
            },
            &mut Vec::new(),
        );
        assert_eq!(scattered, reaffirmed);
    }

    #[test]
    fn crust_field_is_pure_and_bounded() {
        let seed = Seed(42).derive(streams::ROOT);
        let field = CrustField::new(
            seed,
            draw_cratons(seed, &TerrainPins::default(), &mut Vec::new()),
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
        let field = CrustField::new(
            seed,
            draw_cratons(seed, &TerrainPins::default(), &mut Vec::new()),
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
        let field = CrustField::new(
            seed,
            draw_cratons(seed, &TerrainPins::default(), &mut Vec::new()),
        );
        let pos = hornvale_kernel::Position {
            x: -120.25,
            y: 45.5,
        };
        let time = hornvale_kernel::WorldTime { day: 3.0 };
        let (lat, lon) = (45.5f64.to_radians(), (-120.25f64).to_radians());
        let p = [lat.cos() * lon.cos(), lat.cos() * lon.sin(), lat.sin()];
        use hornvale_kernel::Field;
        assert_eq!(field.sample(pos, time), field.thickness_at(p).get());
    }
}
