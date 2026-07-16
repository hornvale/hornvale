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
    // Canonicalize enumeration order: the nested loop above already visits
    // pairs in ascending `(i, j)` craton-slice order, which coincides with
    // ascending `(a, b)` only when the input slice itself is id-ascending.
    // Sort explicitly so `seams` is ascending `(a, b)` regardless of what
    // order the caller's craton slice is in (Task 4 review carry-over).
    seams.sort_by_key(|s| (s.a, s.b));
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

/// Half-width, in `seam_side` units (envelope-difference), over which the
/// clip falls from 1 (fully present) to 0 (fully cut): at `side_toward_self
/// == CLIP_TAPER` the clip is 1, at `-CLIP_TAPER` it is 0, and at exactly 0
/// (the shared seam curve, `seam_side`'s zero set) it is 0.5 — the
/// conjugate margin's own crossing point, by construction (spec §8).
/// type-audit: bare-ok(ratio)
pub const CLIP_TAPER: f64 = 0.08;

/// Smoothstep on `x` clamped to `[0, 1]`: `3x² - 2x³`. A plain cubic
/// ease, not centered — callers center it (`clip_with_rotation` centers on
/// `side_toward_self == 0`).
fn smoothstep(x: f64) -> f64 {
    let x = x.clamp(0.0, 1.0);
    3.0 * x * x - 2.0 * x * x * x
}

/// The assembly-frame clip evaluation shared by `clip_at` (which derives
/// `rotation` fresh from `rift`/`cratons`) and `CrustField` (which
/// precomputes and stores one rotation per major craton at construction —
/// mirroring how `lobing_seeds` precomputes per-craton hash seeds rather
/// than re-deriving them on every sample — and calls this directly to
/// avoid repeating `rotation_for`'s work on every one of the millions of
/// samples a rendered field takes). `p_final` is inverse-rotated into the
/// assembly frame by `rotation`, then every seam touching `craton_id` is
/// evaluated there: `clip = min` over those seams of the smoothstep,
/// centered so the shared seam curve (`seam_side == 0`) reads exactly 0.5.
/// A craton with no seams returns 1.0 (nothing cuts it).
pub(crate) fn clip_with_rotation(
    rift: &RiftHistory,
    cratons: &[Craton],
    craton_id: u32,
    rotation: ([f64; 3], f64),
    p_final: [f64; 3],
) -> f64 {
    let indices = seam_indices_for(rift, craton_id);
    clip_over_seams(rift, cratons, craton_id, rotation, p_final, &indices)
}

/// The indices into `rift.seams` of every seam touching `craton_id`, in
/// seam order. `clip_with_rotation` derives this per call (fine for tests
/// and one-off queries); `CrustField` precomputes it once per major craton
/// at construction — mirroring `lobing_seeds`/`craton_rotations` — so
/// per-sample clipping neither scans the full seam list nor allocates.
/// Byte-identical either way: the filter preserves seam order, and the clip
/// is a min-fold over exactly these seams.
pub(crate) fn seam_indices_for(rift: &RiftHistory, craton_id: u32) -> Vec<usize> {
    rift.seams
        .iter()
        .enumerate()
        .filter(|(_, s)| s.a == craton_id || s.b == craton_id)
        .map(|(i, _)| i)
        .collect()
}

/// Safety margin added to the fracture-noise bound in
/// `SATURATION_SKIP_ENV_DIFF`. `sphere_fbm01` is [0, 1) in exact
/// arithmetic (verified from source: `noise::lattice` returns an exact
/// 53-bit fraction in [0, 1 - 2^-53]; `value_noise_2d`'s bilinear lerp and
/// `fbm_2d`'s exact-amplitude normalization can escape the unit interval
/// only by float-rounding ulps, conservatively < 1e-14), so this margin —
/// five orders of magnitude above the worst rounding escape, and utterly
/// negligible against the ~0.255 threshold it pads — makes the skip bound
/// safe even against every accumulated rounding in the side expression's
/// own combination arithmetic.
const NOISE_ESCAPE_MARGIN: f64 = 1e-9;

/// The plain-envelope difference (toward self) at or above which a seam's
/// clip contribution is EXACTLY 1.0 regardless of the fracture noise, so
/// the (expensive, 12-eval) `sphere_fbm01` sample can be skipped
/// bit-identically: `side_toward_self = env_diff_toward_self +
/// FRACTURE_AMP * (fbm01 - 0.5)` with `fbm01` in [0, 1) (+/- rounding, see
/// `NOISE_ESCAPE_MARGIN`), so `side >= env_diff - FRACTURE_AMP * (0.5 +
/// NOISE_ESCAPE_MARGIN)`; and `smoothstep(0.5 + side / (2 * CLIP_TAPER))`
/// clamps to exactly 1.0 for every `side >= CLIP_TAPER` (the clamp lands
/// on exactly 1.0, where `3.0 - 2.0 = 1.0` is exact), while a min-fold
/// with an exactly-1.0 contribution is the identity. Threshold:
/// `CLIP_TAPER + FRACTURE_AMP * (0.5 + NOISE_ESCAPE_MARGIN)` ~ 0.255.
const SATURATION_SKIP_ENV_DIFF: f64 = CLIP_TAPER + FRACTURE_AMP * (0.5 + NOISE_ESCAPE_MARGIN);

/// The clip evaluation proper, over a precomputed seam-index subset (every
/// seam touching `craton_id`, in seam order — `seam_indices_for`'s output).
/// Identical arithmetic to the former inline loop in `clip_with_rotation`
/// — inverse-rotate into the assembly frame, min-fold the centered
/// smoothstep over the given seams, 1.0 when the subset is empty — with
/// one byte-identity-preserving fast path: when the noise-free
/// plain-envelope bound already saturates the smoothstep at exactly 1.0
/// (`SATURATION_SKIP_ENV_DIFF`), the fracture noise is never sampled and
/// the seam is skipped (min-fold with 1.0 is the identity). The slow path
/// mirrors `seam_side`'s arithmetic operation-for-operation (same envelope
/// calls in the same order, same combination expression), so its `raw` is
/// bit-equal to `seam_side`'s — `seam_side` itself stays the battery-facing
/// definition of the margin function, untouched (the saturation is a
/// property of the CLIP's clamp, not of `seam_side`'s value; the
/// conjugate-fit battery keeps bisecting true `seam_side`).
pub(crate) fn clip_over_seams(
    rift: &RiftHistory,
    cratons: &[Craton],
    craton_id: u32,
    rotation: ([f64; 3], f64),
    p_final: [f64; 3],
    seam_indices: &[usize],
) -> f64 {
    let (pole, angle) = rotation;
    let p_assembly = rotate(pole, -angle, p_final);
    let mut clip = 1.0_f64;
    let mut touched = false;
    for &si in seam_indices {
        let seam = &rift.seams[si];
        touched = true;
        // The two plain-envelope terms of `seam_side`, computed in its
        // exact order (a then b, assembly centers, same radii).
        let ia = craton_index(seam.a, cratons);
        let ib = craton_index(seam.b, cratons);
        let env_a = plain_envelope(rift.assembly[ia], cratons[ia].radius_rad, p_assembly);
        let env_b = plain_envelope(rift.assembly[ib], cratons[ib].radius_rad, p_assembly);
        let env_diff = env_a - env_b;
        let env_diff_toward_self = if seam.a == craton_id {
            env_diff
        } else {
            -env_diff
        };
        if env_diff_toward_self >= SATURATION_SKIP_ENV_DIFF {
            // Contribution is exactly 1.0 (see SATURATION_SKIP_ENV_DIFF's
            // proof); min-fold with 1.0 is the identity — skip the noise.
            continue;
        }
        // Slow path: `seam_side`'s own expression, bit-for-bit
        // (`env_a - env_b + FRACTURE_AMP * (fracture - 0.5)`).
        let fracture = sphere_fbm01(seam.noise_seed, p_assembly, FRACTURE_FREQ, FRACTURE_OCTAVES);
        let raw = env_a - env_b + FRACTURE_AMP * (fracture - 0.5);
        let side_toward_self = if seam.a == craton_id { raw } else { -raw };
        clip = clip.min(smoothstep(0.5 + side_toward_self / (2.0 * CLIP_TAPER)));
    }
    if touched { clip } else { 1.0 }
}

/// The clip factor for craton `craton_id` at a FINAL-frame point
/// `p_final`: inverse-rotate by `craton_id`'s own assembly-to-final
/// rotation (`rotation_for(rift.assembly[idx], cratons[idx].center)`,
/// `idx` being `craton_id`'s position in `cratons`) and evaluate every
/// seam touching it (`clip_with_rotation`). Standalone entry point for
/// tests and any caller without a precomputed rotation cache; `CrustField`
/// itself calls `clip_with_rotation` directly with a rotation it
/// precomputed once at construction (see that function's doc).
/// type-audit: bare-ok(index: craton_id), bare-ok(ratio: p_final), bare-ok(ratio: return)
pub fn clip_at(rift: &RiftHistory, cratons: &[Craton], craton_id: u32, p_final: [f64; 3]) -> f64 {
    let idx = craton_index(craton_id, cratons);
    let rotation = rotation_for(rift.assembly[idx], cratons[idx].center);
    clip_with_rotation(rift, cratons, craton_id, rotation, p_final)
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
    fn clip_is_one_far_from_seams_and_single_craton_is_uncut() {
        // A single craton has nothing to seam against — `draw_rift`
        // produces zero seams for it, so `clip_at` is 1.0 everywhere,
        // interior or not.
        let craton = Craton {
            id: 0,
            center: [0.0, 0.0, 1.0],
            radius_rad: 0.4,
            age: 0.5,
        };
        let cratons = vec![craton.clone()];
        let terrain_seed = Seed(5).derive(streams::ROOT);
        let rift = draw_rift(terrain_seed, &cratons);
        assert!(rift.seams.is_empty(), "a lone craton must not seam");
        for p in [
            craton.center,
            normalize([1.0, 0.0, 0.0]),
            normalize([0.3, 0.4, 0.8]),
        ] {
            assert_eq!(clip_at(&rift, &cratons, 0, p), 1.0);
        }

        // A real multi-craton assembly: deep at a seaming craton's own
        // assembly center, `side_toward_self` is strongly positive (its
        // own envelope near 1, the neighbor's near 0) — comfortably past
        // `CLIP_TAPER`'s taper band, so the clip saturates to 1.0.
        let cratons = draw_test_cratons(42);
        let terrain_seed = Seed(42).derive(streams::ROOT);
        let rift = draw_rift(terrain_seed, &cratons);
        let seam = rift
            .seams
            .first()
            .expect("seed 42's assembly must produce at least one seam");
        let ia = craton_index(seam.a, &cratons);
        assert_eq!(
            clip_at(&rift, &cratons, seam.a, cratons[ia].center),
            1.0,
            "clip did not saturate deep in the winning craton's own interior"
        );
    }

    /// Bisect `seam_side` to zero along the cross-arc through `base`
    /// (perpendicular to the a-b great-circle direction at `base`,
    /// `perp`), within `[-search_range, search_range]` radians. `None`
    /// when the two endpoints don't bracket a root (no sign change) — the
    /// caller (the conjugate-fit battery) treats that step as unusable
    /// rather than asserting on a spurious point.
    fn bisect_seam_zero(
        seam: &Seam,
        cratons: &[Craton],
        assembly: &[[f64; 3]],
        base: [f64; 3],
        perp: [f64; 3],
        search_range: f64,
    ) -> Option<[f64; 3]> {
        let f = |angle: f64| {
            let p = crate::plates::rotate_toward(base, perp, angle);
            seam_side(seam, cratons, assembly, p)
        };
        let (mut lo, mut hi) = (-search_range, search_range);
        let (mut f_lo, f_hi) = (f(lo), f(hi));
        if f_lo == 0.0 {
            return Some(crate::plates::rotate_toward(base, perp, lo));
        }
        if f_hi == 0.0 {
            return Some(crate::plates::rotate_toward(base, perp, hi));
        }
        if f_lo.signum() == f_hi.signum() {
            return None;
        }
        for _ in 0..64 {
            let mid = 0.5 * (lo + hi);
            let f_mid = f(mid);
            if f_mid.signum() == f_lo.signum() {
                lo = mid;
                f_lo = f_mid;
            } else {
                hi = mid;
            }
        }
        Some(crate::plates::rotate_toward(base, perp, 0.5 * (lo + hi)))
    }

    #[test]
    fn conjugate_margins_fit_by_construction() {
        // A hand-placed, isolated two-craton pair rather than a drawn
        // multi-craton assembly (`draw_test_cratons`): seed 42's assembly
        // has cratons of degree > 1 (e.g. craton 0 seams 1, 2, AND 3), so
        // `clip_at`'s min-over-seams would fold in a SECOND seam's
        // (unrelated) zero set along the walk below and the 0.5 read
        // would not isolate this seam's own fit. Two cratons in contact
        // guarantees exactly one seam, so every curve point found is
        // governed by that seam alone on both sides.
        let a = Craton {
            id: 0,
            center: [1.0, 0.0, 0.0],
            radius_rad: 0.3,
            age: 0.0,
        };
        let b_center = normalize([math::cos(0.5), math::sin(0.5), 0.0]);
        let b = Craton {
            id: 1,
            center: b_center,
            radius_rad: 0.3,
            age: 0.0,
        };
        let cratons = vec![a, b];
        let terrain_seed = Seed(1).derive(streams::ROOT);
        let rift = draw_rift(terrain_seed, &cratons);
        assert_eq!(rift.seams.len(), 1, "the pair must seam exactly once");
        let seam = &rift.seams[0];
        let ia = craton_index(seam.a, &cratons);
        let ib = craton_index(seam.b, &cratons);
        let center_a = rift.assembly[ia];
        let center_b = rift.assembly[ib];
        let axis = normalize(cross(center_a, center_b));
        let total_angle = math::acos(dot(center_a, center_b).clamp(-1.0, 1.0));
        // Wide enough to bracket the fracture-noise-wobbled zero set
        // (`FRACTURE_AMP` can shift it well off the plain-envelope
        // midpoint) without straying past either craton's own center.
        let search_range = 0.9 * (cratons[ia].radius_rad + cratons[ib].radius_rad);

        // Each craton's own assembly -> final rotation (Task 5 mapping:
        // `rotation_for(assembly[idx], cratons[idx].center)`).
        let (pole_a, angle_a) = rotation_for(center_a, cratons[ia].center);
        let (pole_b, angle_b) = rotation_for(center_b, cratons[ib].center);

        // Walk the great circle between the two assembly centers; at each
        // step, bisect perpendicular to it for the seam's zero crossing.
        // The set of found `q`s is ONE shared assembly-frame curve — both
        // `seam_side(seam, ..., q) == 0`, by construction of the bisection
        // — which is exactly the point of the battery: two cratons, two
        // separate forward rotations, but the SAME curve underneath both
        // margins.
        let mut curve_points = Vec::new();
        for i in 0..50 {
            let t = (i as f64 + 1.0) / 51.0; // stay off the exact centers
            let base = rotate(axis, t * total_angle, center_a);
            let direction = normalize(cross(axis, base));
            let perp = normalize(cross(base, direction));
            if let Some(q) =
                bisect_seam_zero(seam, &cratons, &rift.assembly, base, perp, search_range)
            {
                curve_points.push(q);
            }
        }
        assert!(
            curve_points.len() >= 30,
            "too few usable curve points ({}) — battery cannot judge the fit off this few",
            curve_points.len()
        );

        for q in curve_points {
            let p_a = rotate(pole_a, angle_a, q);
            let clip_a = clip_at(&rift, &cratons, seam.a, p_a);
            assert!(
                (clip_a - 0.5).abs() < 1e-6,
                "craton a's clip at its own forward-rotated margin curve is {clip_a}, not 0.5"
            );
            let p_b = rotate(pole_b, angle_b, q);
            let clip_b = clip_at(&rift, &cratons, seam.b, p_b);
            assert!(
                (clip_b - 0.5).abs() < 1e-6,
                "craton b's clip at its own forward-rotated margin curve is {clip_b}, not 0.5"
            );
        }
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
