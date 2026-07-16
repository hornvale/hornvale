//! Rift: the drawn rift history over an assembled craton set (rift-and-fit,
//! spec §3.2/§3.4). Still additive as of Task 4 — nothing in `generate`
//! calls `draw_rift` yet, so no world's bytes change; this module only
//! introduces the `terrain/rift` stream and the seam/rotation primitives
//! Task 6 wires in.
//!
//! A seam is the drawn contact between two cratons at the *assembly*
//! configuration (`crust::assemble_cratons`) — the pre-rift, sutured
//! layout. `seam_side` reads as a signed margin function of that
//! geometry: positive on one craton's side, negative on the other's, with
//! a fracture-noise wobble so the seam is not a perfect great-circle arc.
//! `rotation_for`/`rotate` carry a craton from its assembly position to
//! wherever the rift ultimately displaces it (a later task's concern —
//! here they are pure, independently testable Euler-pole primitives).

use crate::crust::{CONTACT_FACTOR, Craton, assemble_cratons, sphere_fbm01};
use crate::plates::{cross, dot, norm, normalize};
use crate::streams;
use hornvale_kernel::{Seed, math};

/// Slack added to `crust::CONTACT_FACTOR` when deciding whether two
/// cratons seam: `assemble_cratons` settles contacts by bisection to
/// within float precision, so a seam test at the bare `CONTACT_FACTOR`
/// threshold can miss a pair whose settled separation lands a hair past
/// it on the wrong side. The slack is one-directional (it can only add
/// pairs, never drop one that would have passed at the bare factor).
/// type-audit: bare-ok(ratio)
pub const SEAM_SLACK: f64 = 0.02;

/// Amplitude of the fracture-noise term blended into `seam_side`: the
/// margin is not a perfect great-circle arc between the two plain
/// envelopes, but wobbles by up to this much (the noise term ranges over
/// `[-FRACTURE_AMP/2, FRACTURE_AMP/2]`). Multi-cell-scale crenulation
/// (a second, finer octave band) is Task 10's banked knob, not this one.
/// type-audit: bare-ok(ratio)
pub const FRACTURE_AMP: f64 = 0.35;

/// Base spatial frequency of the seam's fracture noise.
/// type-audit: bare-ok(ratio)
pub const FRACTURE_FREQ: f64 = 6.0;

/// fBm octaves for the fracture noise (matches `crust::LOBE_OCTAVES`'s
/// shape; kept private — not a knob Task 10 exposes yet).
const FRACTURE_OCTAVES: u32 = 4;

/// Lower bound of the drawn spreading-rate range, rad-per-unit-age. A
/// narrative scalar only — nothing yet converts it to a physical rate;
/// it exists so downstream prose ("the rift widened slowly/quickly") has
/// a seeded number to read.
/// type-audit: bare-ok(ratio)
pub const SPREAD_MIN: f64 = 0.3;

/// Upper bound of the drawn spreading-rate range.
/// type-audit: bare-ok(ratio)
pub const SPREAD_MAX: f64 = 1.2;

/// A rift seam between two cratons that touch (or nearly touch) at the
/// assembly configuration (spec §3.2). `a < b` always — seams are
/// canonicalized by craton id, independent of draw order, so two seams
/// over the same pair are always equal regardless of which craton was
/// enumerated first.
/// type-audit: bare-ok(index: a), bare-ok(index: b)
#[derive(Debug, Clone, PartialEq)]
pub struct Seam {
    /// The lower-id craton across the seam.
    pub a: u32,
    /// The higher-id craton across the seam (`b > a`).
    pub b: u32,
    /// Hash-derived seed for this seam's fracture noise
    /// (`seam-{a}-{b}`, sub-derived off the `terrain/rift` stream root).
    /// A pure function of `(a, b)`: independent of enumeration order and
    /// of how many other seams exist, so the seam count can change
    /// (e.g. a different craton layout) without perturbing any other
    /// seam's noise.
    pub noise_seed: Seed,
}

/// The drawn rift history over one craton set (spec §3.2/§3.4): the
/// assembly frame it seamed against, every seam found, and one global
/// spreading-rate scalar.
/// type-audit: bare-ok(ratio: assembly), bare-ok(ratio: spreading_rate)
#[derive(Debug, Clone, PartialEq)]
pub struct RiftHistory {
    /// Contact-configuration assembly centers (`crust::assemble_cratons`'s
    /// output), index-aligned with the craton slice `draw_rift` was
    /// called with.
    pub assembly: Vec<[f64; 3]>,
    /// Every seam between assembly-adjacent cratons in the input set,
    /// ascending `(a, b)`.
    pub seams: Vec<Seam>,
    /// One global spreading rate, rad-per-unit-age (narrative scalar
    /// only — see `SPREAD_MIN`/`SPREAD_MAX`).
    pub spreading_rate: f64,
}

/// Draw the rift history for `cratons`: assemble them (`assemble_cratons`,
/// draw-free), find every seam among assembly-adjacent pairs, and draw
/// one global spreading rate.
///
/// `cratons` is whatever set the caller passes — `draw_rift` seams
/// exactly that set; it does not itself distinguish majors from
/// microcontinents or re-derive the craton list. (Wiring which set
/// `generate` passes is Task 6's concern.)
///
/// Stream discipline (`terrain/rift`, epoch v4 save-format contract):
/// exactly one sequential `next_f64` draw, for `spreading_rate` — the
/// seam count can vary freely (more or fewer cratons, a different
/// layout) without perturbing that draw, because every seam's geometry
/// comes from a hash sub-derivation (`seam-{a}-{b}`) off the same stream
/// root, never from the sequential stream itself. See `streams::RIFT`'s
/// doc and the pin-isolation discipline it cites.
pub fn draw_rift(terrain_seed: Seed, cratons: &[Craton]) -> RiftHistory {
    let assembly = assemble_cratons(cratons);
    let rift_root = terrain_seed.derive(streams::RIFT);
    let mut seams = Vec::new();
    for i in 0..cratons.len() {
        for j in (i + 1)..cratons.len() {
            let separation = math::acos(dot(assembly[i], assembly[j]).clamp(-1.0, 1.0));
            let threshold =
                (CONTACT_FACTOR + SEAM_SLACK) * (cratons[i].radius_rad + cratons[j].radius_rad);
            if separation > threshold {
                continue;
            }
            let (a, b) = if cratons[i].id < cratons[j].id {
                (cratons[i].id, cratons[j].id)
            } else {
                (cratons[j].id, cratons[i].id)
            };
            let noise_seed = rift_root.derive(&format!("seam-{a}-{b}"));
            seams.push(Seam { a, b, noise_seed });
        }
    }
    let mut stream = rift_root.stream();
    let spreading_rate = SPREAD_MIN + (SPREAD_MAX - SPREAD_MIN) * stream.next_f64();
    RiftHistory {
        assembly,
        seams,
        spreading_rate,
    }
}

/// Find `id`'s position in `cratons`. Panics if absent — `Seam`s are only
/// ever constructed by `draw_rift` from the same slice a caller then
/// queries `seam_side` against, so a missing id means the caller passed a
/// different (or mismatched) craton set, a programming error worth
/// failing loudly on rather than silently misreading geometry.
fn craton_index(id: u32, cratons: &[Craton]) -> usize {
    cratons
        .iter()
        .position(|c| c.id == id)
        .unwrap_or_else(|| panic!("seam craton id {id} missing from cratons"))
}

/// The plain (unlobed) quartic envelope of one craton at `p`: 1 at
/// `center`, tapering to 0 at `radius_rad`, clamped — deliberately not
/// `crust::lobed_envelope`. A seam must be a pure function of the drawn
/// geometry (center, radius) alone, not of the independent per-craton
/// lobing noise: two cratons sharing a seam are lobed with two different
/// seeds, so a lobed margin would not be the same curve read from either
/// side, and the conjugate fit (Task 5) needs exactly one shared curve.
fn plain_envelope(center: [f64; 3], radius_rad: f64, p: [f64; 3]) -> f64 {
    let angle = math::acos(dot(center, p).clamp(-1.0, 1.0));
    let x = (angle / radius_rad).clamp(0.0, 1.0);
    (1.0 - x * x).powi(2)
}

/// Signed margin function shared by both sides of `seam`: positive on
/// craton `a`'s side, negative on `b`'s. Defined as
/// `env_a(p) - env_b(p) + FRACTURE_AMP * (fbm01(seam.noise_seed, p) - 0.5)`,
/// where `env_x` is the plain envelope (`plain_envelope`) of craton `x`
/// evaluated against its ASSEMBLY center (not its drawn center) — the
/// contact-configuration geometry `draw_rift` seamed against. The fBm term
/// is sampled at the 3-D point `p` itself (spherical fBm at the
/// assembly-frame position, `crust::sphere_fbm01`'s house pattern) rather
/// than along a 1-D parameterization of the seam's arc-length: simpler and
/// rotation-stable, since it needs no arc-length parameterization that
/// would itself have to survive whatever later rotates a craton off its
/// assembly position.
/// type-audit: bare-ok(ratio)
pub fn seam_side(seam: &Seam, cratons: &[Craton], assembly: &[[f64; 3]], p: [f64; 3]) -> f64 {
    let ia = craton_index(seam.a, cratons);
    let ib = craton_index(seam.b, cratons);
    let env_a = plain_envelope(assembly[ia], cratons[ia].radius_rad, p);
    let env_b = plain_envelope(assembly[ib], cratons[ib].radius_rad, p);
    let fracture = sphere_fbm01(seam.noise_seed, p, FRACTURE_FREQ, FRACTURE_OCTAVES);
    env_a - env_b + FRACTURE_AMP * (fracture - 0.5)
}

/// The Euler pole and angle carrying `assembly_center` to `final_center`
/// along the shorter great-circle arc: pole = the normalized cross
/// product, angle = the arc's angular length. When the two centers are
/// parallel or antipodal (`‖cross‖ < 1e-9`), no pole is privileged — an
/// antipodal displacement has infinitely many great circles connecting
/// its endpoints, and a coincident pair has none — so the deterministic
/// choice is the identity rotation, `([0, 0, 1], 0.0)`, leaving the
/// craton unrotated rather than picking an arbitrary axis.
/// type-audit: bare-ok(ratio)
pub fn rotation_for(assembly_center: [f64; 3], final_center: [f64; 3]) -> ([f64; 3], f64) {
    let raw = cross(assembly_center, final_center);
    if norm(raw) < 1e-9 {
        return ([0.0, 0.0, 1.0], 0.0);
    }
    let pole = normalize(raw);
    let angle = math::acos(dot(assembly_center, final_center).clamp(-1.0, 1.0));
    (pole, angle)
}

/// Rotate `p` by `angle` radians about unit axis `pole` (Rodrigues'
/// rotation formula). The inverse of `rotate(pole, angle, _)` is
/// `rotate(pole, -angle, _)` — negating the angle reverses the turn
/// about the same fixed axis.
/// type-audit: bare-ok(ratio)
pub fn rotate(pole: [f64; 3], angle: f64, p: [f64; 3]) -> [f64; 3] {
    let (c, s) = (math::cos(angle), math::sin(angle));
    let k_cross_p = cross(pole, p);
    let k_dot_p = dot(pole, p);
    [
        p[0] * c + k_cross_p[0] * s + pole[0] * k_dot_p * (1.0 - c),
        p[1] * c + k_cross_p[1] * s + pole[1] * k_dot_p * (1.0 - c),
        p[2] * c + k_cross_p[2] * s + pole[2] * k_dot_p * (1.0 - c),
    ]
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::pins::TerrainPins;
    use crate::plates::norm as vec_norm;

    /// The default-pins ocean-fraction target for a given terrain seed —
    /// mirrors `crust::tests::default_ocean_target` (that helper is
    /// module-private, so `draw_cratons`-based tests here need their own
    /// copy).
    fn default_ocean_target(terrain_seed: Seed) -> f64 {
        crate::elevation::resolve_ocean_fraction(
            terrain_seed,
            &TerrainPins::default(),
            &mut Vec::new(),
        )
    }

    fn draw_test_cratons(seed_val: u64) -> Vec<Craton> {
        let terrain_seed = Seed(seed_val).derive(streams::ROOT);
        let ocean_target = default_ocean_target(terrain_seed);
        crate::crust::draw_cratons(
            terrain_seed,
            &TerrainPins::default(),
            ocean_target,
            &mut Vec::new(),
        )
    }

    #[test]
    fn rift_draws_one_scalar_and_derives_seams_per_pair() {
        let seed = Seed(42).derive(crate::streams::ROOT);
        let cratons = draw_test_cratons(42);
        let rift = draw_rift(seed, &cratons);
        assert!((SPREAD_MIN..=SPREAD_MAX).contains(&rift.spreading_rate));
        assert!(!rift.seams.is_empty(), "a multi-craton assembly must seam");
        for s in &rift.seams {
            assert!(s.a < s.b);
        }
        // Per-pair stability: a seam's noise seed depends only on (a, b) —
        // recompute and compare.
        assert_eq!(draw_rift(seed, &cratons), rift);
    }

    #[test]
    fn seam_count_and_spreading_rate_are_stable_across_seeds() {
        for seed_val in [1u64, 7, 42, 99] {
            let cratons = draw_test_cratons(seed_val);
            let terrain_seed = Seed(seed_val).derive(streams::ROOT);
            let a = draw_rift(terrain_seed, &cratons);
            let b = draw_rift(terrain_seed, &cratons);
            assert_eq!(a, b, "seed {seed_val}: draw_rift is not deterministic");
        }
    }

    #[test]
    fn seam_side_is_shared_and_signed() {
        let cratons = draw_test_cratons(42);
        let terrain_seed = Seed(42).derive(crate::streams::ROOT);
        let rift = draw_rift(terrain_seed, &cratons);
        let seam = rift
            .seams
            .first()
            .expect("seed 42's assembly must produce at least one seam");
        let ia = craton_index(seam.a, &cratons);
        let ib = craton_index(seam.b, &cratons);
        // At craton a's assembly center: positive.
        assert!(seam_side(seam, &cratons, &rift.assembly, rift.assembly[ia]) > 0.0);
        // At craton b's assembly center: negative.
        assert!(seam_side(seam, &cratons, &rift.assembly, rift.assembly[ib]) < 0.0);
    }

    #[test]
    fn seam_sign_flips_and_zero_set_moves_with_noise_seed() {
        let cratons = draw_test_cratons(42);
        let terrain_seed = Seed(42).derive(crate::streams::ROOT);
        let rift = draw_rift(terrain_seed, &cratons);
        assert!(
            rift.seams.len() >= 2,
            "need at least two seams to compare noise curves"
        );
        for seam in &rift.seams {
            let ia = craton_index(seam.a, &cratons);
            let ib = craton_index(seam.b, &cratons);
            assert!(seam_side(seam, &cratons, &rift.assembly, rift.assembly[ia]) > 0.0);
            assert!(seam_side(seam, &cratons, &rift.assembly, rift.assembly[ib]) < 0.0);
        }
        // Two different (a, b) pairs draw different noise seeds, so the
        // margin curve differs between them: sampled across a shared set
        // of points, the two seams' signed values are not identical.
        let (s0, s1) = (&rift.seams[0], &rift.seams[1]);
        assert_ne!(s0.noise_seed, s1.noise_seed);
        let sample_points = &rift.assembly;
        let differs = sample_points.iter().any(|&p| {
            seam_side(s0, &cratons, &rift.assembly, p) != seam_side(s1, &cratons, &rift.assembly, p)
        });
        assert!(differs, "two seams produced identical margin curves");
    }

    #[test]
    fn rotation_round_trips_via_inverse_angle() {
        let cases = [
            ([1.0, 0.0, 0.0], [0.0, 1.0, 0.0]),
            ([0.0, 0.0, 1.0], [1.0, 0.0, 0.0]),
            ([0.6, 0.8, 0.0], normalize([0.1, 0.2, 0.9])),
        ];
        for (from, to) in cases {
            let (pole, angle) = rotation_for(from, to);
            assert!((vec_norm(pole) - 1.0).abs() < 1e-9 || angle == 0.0);
            let rotated = rotate(pole, angle, from);
            // rotate(pole, angle, from) should land on `to` (within
            // float precision) when a real pole was found.
            if angle != 0.0 {
                for k in 0..3 {
                    assert!(
                        (rotated[k] - to[k]).abs() < 1e-9,
                        "rotation did not carry from -> to: {rotated:?} vs {to:?}"
                    );
                }
            }
            // Rodrigues round-trip: rotate then inverse-rotate returns
            // the original point within 1e-12.
            let back = rotate(pole, -angle, rotated);
            for k in 0..3 {
                assert!(
                    (back[k] - from[k]).abs() < 1e-12,
                    "round-trip failed at {k}"
                );
            }
        }
    }

    #[test]
    fn rotation_for_near_parallel_is_identity() {
        let p = normalize([0.3, 0.4, 0.5]);
        let (pole, angle) = rotation_for(p, p);
        assert_eq!(pole, [0.0, 0.0, 1.0]);
        assert_eq!(angle, 0.0);
        let antipodal = [-p[0], -p[1], -p[2]];
        let (pole2, angle2) = rotation_for(p, antipodal);
        assert_eq!(pole2, [0.0, 0.0, 1.0]);
        assert_eq!(angle2, 0.0);
    }
}
