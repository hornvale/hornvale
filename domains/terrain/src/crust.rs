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
    let centered = 0.5 * (REBALANCE_GAIN * (n - 0.5)).tanh();
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
        ((1.0 - t) * omega).sin() / omega.sin(),
        (t * omega).sin() / omega.sin(),
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
/// `--supercontinent` is applied after every position is drawn and every
/// radius rescaled: it clusters cratons 1.. toward craton 0 by `slerp`,
/// so the pin perturbs positions but never the draw sequence or the area
/// normalization (pin isolation again).
///
/// Repulsion (Task 9 iteration 2, retargeted in iteration 3') is applied
/// last, and only when the world is *not* pinned `--supercontinent` (that
/// pin wants clustering — repelling what it just pulled together would
/// fight it): one deterministic pass, in id order, zero stream draws,
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

/// The continental fraction of a craton's nominal cap: `1 - sqrt(e)`,
/// where `e` is the envelope value at which crust thickness exactly
/// crosses `CONTINENTAL_THRESHOLD_KM` (see `draw_cratons`'s doc for the
/// derivation). `peak_km` is the same age-derived peak `thickness_at`
/// computes.
fn continental_cap_fraction(peak_km: f64) -> f64 {
    let e = (CONTINENTAL_THRESHOLD_KM - OCEANIC_KM) / (peak_km - OCEANIC_KM);
    1.0 - e.sqrt()
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
    let continental_area: f64 = cratons
        .iter()
        .map(|c| {
            let peak = PEAK_MIN_KM + (PEAK_MAX_KM - PEAK_MIN_KM) * (1.0 - c.age);
            std::f64::consts::TAU * (1.0 - c.radius_rad.cos()) * continental_cap_fraction(peak)
        })
        .sum();
    if continental_area > 0.0 {
        let scale = ((budget * 4.0 * std::f64::consts::PI) / continental_area).sqrt();
        for c in cratons.iter_mut() {
            c.radius_rad = (c.radius_rad * scale).min(0.6);
        }
    }
    if pins.supercontinent == Some(true) && cratons.len() > 1 {
        let anchor = cratons[0].center;
        for craton in cratons.iter_mut().skip(1) {
            craton.center = slerp(craton.center, anchor, CLUSTER_PULL);
        }
    }
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
                let omega = dot(c_i, c_j).clamp(-1.0, 1.0).acos();
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
}

impl CrustField {
    /// Assemble the field over a drawn craton set.
    pub fn new(terrain_seed: Seed, cratons: Vec<Craton>) -> CrustField {
        let lobing_root = terrain_seed.derive(streams::CRATONS).derive("lobing");
        let lobing_seeds = cratons
            .iter()
            .map(|c| lobing_root.derive(&format!("craton-{}", c.id)))
            .collect();
        CrustField {
            cratons,
            lobing_seeds,
        }
    }

    /// Envelope and winning craton at a position.
    fn strongest(&self, p: [f64; 3]) -> Option<(f64, &Craton)> {
        self.cratons
            .iter()
            .map(|c| {
                let seed = self.lobing_seeds[c.id as usize];
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
                    let angle = dot(cratons[i].center, cratons[j].center)
                        .clamp(-1.0, 1.0)
                        .acos();
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
    fn supercontinent_clusters_cratons_without_new_draws() {
        let seed = Seed(42).derive(streams::ROOT);
        let ocean_target = default_ocean_target(seed);
        let scattered = draw_cratons(seed, &TerrainPins::default(), ocean_target, &mut Vec::new());
        let clustered = draw_cratons(
            seed,
            &TerrainPins {
                supercontinent: Some(true),
                ..TerrainPins::default()
            },
            ocean_target,
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
        let p = [lat.cos() * lon.cos(), lat.cos() * lon.sin(), lat.sin()];
        use hornvale_kernel::Field;
        assert_eq!(field.sample(pos, time), field.thickness_at(p).get());
    }
}
