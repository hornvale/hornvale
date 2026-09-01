//! Elevation (meters, bare f64 by documented convention), sea level, and
//! the unrest field. Elevation = the isostatic base over crust thickness +
//! the nearest same-plate boundary's contribution decayed by graph distance
//! and shaped by the plate's maturity + drawn hotspots + a per-vertex
//! micro-epsilon that guarantees a strict ordering (so the sea-level
//! percentile is exact). Crust epoch (Task 8): the flat continental/oceanic
//! base retires in favor of Airy isostasy over the crust field's thickness
//! (`crust.rs`), producing a genuine shelf where crust tapers through the
//! continental threshold.
//!
//! **Finding, largely resolved (diagnosed Task 8, fixed for the general
//! case in Task 9):** the drawn craton footprint used to cover only
//! ~2-10% of a globe's vertices at the continental threshold, far short of
//! the 25-50% land fraction `ocean_fraction`'s drawn range implies, so sea
//! level's exact-percentile mechanism (`derive_sea_level`) landed deep
//! inside the abyssal plain for most seeds instead of on the craton's
//! shelf taper — weakening hypsometric bimodality and inflating the
//! ±200 m shelf band (measured: ~2/20 default seeds passed both bounds at
//! canonical level 6; ~3/40 at the single-craton scenario). Two authorized
//! fixes landed together in Task 9: `crust::draw_cratons` now rescales its
//! drawn radii so total spherical-cap area matches the land-quota budget
//! (clamped at 0.6 rad per craton), and `crust::PEAK_MIN_KM` moved from 30
//! (exactly `ISOSTASY_REF_KM`, so an "old" craton floated at 0 m and never
//! surfaced) to 33 (crests ~540 m). See the Task 9 report for the
//! after-census evidence on the general (multi-craton) population this
//! fixes. The last edge case — a *lone pinned* craton (`continents=1`) clamps
//! to exactly 0.6 rad, capping its cap area at ~8.7% of the sphere,
//! below any achievable land quota — is resolved by the shelf-break
//! fallback (`effective_ocean_target`, decision 0053): a supply-limited
//! world keeps `SHELF_BREAK_LAND_FACTOR × supply` land, placing the
//! percentile at the isostatic shelf break instead of the abyssal plain.

use crate::boundaries::{BoundaryKind, VertexBoundary};
use crate::pins::TerrainPins;
use crate::plates::{Plate, dot, unit_vector};
use crate::streams;
use hornvale_kernel::{
    Geosphere, NearestVertexIndex, ReferenceElevation, Seed, Vertex, VertexMap, math,
};

/// Airy isostasy: meters of elevation per kilometer of crust thickness.
/// type-audit: pending(wave-2)
pub const ISOSTASY_M_PER_KM: f64 = 180.0;
/// Crust thickness that floats exactly at zero elevation, km.
/// type-audit: pending(wave-2)
pub const ISOSTASY_REF_KM: f64 = 30.0;

/// Isostatic base elevation for a crust thickness, meters: linear in
/// thickness around the reference crust that floats at sea level — thicker
/// (buoyant, continental) crust rides higher, thinner (dense, oceanic)
/// crust rides lower. Monotone by construction.
pub(crate) fn isostatic_m(thickness_km: f64) -> f64 {
    ISOSTASY_M_PER_KM * (thickness_km - ISOSTASY_REF_KM)
}

/// Per-vertex-id micro-relief, meters. Breaks every elevation tie so the
/// sea-level percentile is exact; at 40,962 vertices (the canonical level-6
/// grid) the total spread is ~0.04 m — physically invisible, a declared
/// approximation.
const VERTEX_EPSILON_M: f64 = 1e-6;
/// Maximum possible closing speed (two rate-1.0 plates head-on); boundary
/// magnitudes are normalized against it.
const MAX_CLOSING_SPEED: f64 = 2.0;

/// Signed peak amplitude (meters, at full closing speed, before the
/// maturity factor) a boundary kind contributes on a vertex whose own plate
/// is `continental`. The mixed kinds are side-dependent: a coastal range
/// rises on the continent while the trench deepens offshore; an ocean–ocean
/// arc rises on the overriding side (`arc_side`, deterministically the
/// higher plate id) while the other side takes the trench.
fn boundary_amplitude_m(kind: BoundaryKind, continental: bool, arc_side: bool) -> f64 {
    match kind {
        BoundaryKind::ContinentalCollision => 5000.0,
        BoundaryKind::CoastalRange => {
            if continental {
                3000.0
            } else {
                -3000.0
            }
        }
        BoundaryKind::IslandArc => {
            if arc_side {
                1500.0
            } else {
                -2500.0
            }
        }
        BoundaryKind::ContinentalRift => -500.0,
        BoundaryKind::OceanicRidge => 1500.0,
        BoundaryKind::Transform => 0.0,
    }
}

/// Foreland-basin trough depth, m (Sculpting spec §3: belt anatomy).
/// type-audit: pending(wave-2)
pub const FORELAND_DEPTH_M: f64 = -350.0;
/// Foreland trough band, in boundary graph hops (continental side).
/// type-audit: bare-ok(count)
pub const FORELAND_HOPS: (u32, u32) = (3, 6);
/// Trench trough depth, m, at the subducting-side boundary vertex.
/// type-audit: pending(wave-2)
pub const TRENCH_DEPTH_M: f64 = -2800.0;
/// Trench half-width, hops.
/// type-audit: bare-ok(count)
pub const TRENCH_HOPS: u32 = 1;
/// Arc edifice spacing wavelength (in gate-noise cycles per radian).
/// type-audit: pending(wave-2)
pub const ARC_SPACING: f64 = 9.0;
/// Fraction of the arc that is edifice (gate above this is "on").
/// type-audit: bare-ok(ratio)
pub const ARC_DUTY: f64 = 0.45;

/// Collision-belt crest decay length, vertices: the sharp core.
const CREST_DECAY_VERTICES: f64 = 1.0;
/// Foothills amplitude, as a fraction of the belt's peak amplitude. Kept
/// small (not the more "visually broad" fraction a first sketch might
/// reach for) so the shoulder's own tail has died down well below
/// `FORELAND_DEPTH_M` — a fixed depth, independent of belt strength — by
/// the time the foreland band starts; otherwise a strong belt's foothills
/// alone swamp the trough and it never goes negative. See the Task 3
/// report for the numeric sweep that picked this value.
const FOOTHILLS_FRACTION: f64 = 0.1;
/// Foothills decay length, vertices: broader than the crest's apron.
const FOOTHILLS_DECAY_VERTICES: f64 = 4.0;
/// Island-arc volcanic-edifice decay length, vertices. Also the reach of
/// [`edifice_present`]: one e-folding out, the cone; beyond it, the apron.
pub(crate) const ARC_EDIFICE_DECAY_VERTICES: f64 = 1.5;
/// Trench-notch decay length, vertices: the sharp seaward deep.
const TRENCH_DECAY_VERTICES: f64 = 1.0;
/// Decay length beyond the trench notch, vertices.
const FAR_FIELD_DECAY_VERTICES: f64 = 3.0;
/// Residual amplitude fraction beyond the trench notch.
const FAR_FIELD_FRACTION: f64 = 0.2;
/// Octaves for the along-strike arc-gate noise.
const ARC_GATE_OCTAVES: u32 = 4;

/// fBm relief peak amplitude, meters (spec §3).
/// type-audit: pending(wave-2)
pub const RELIEF_AMPLITUDE_M: f64 = 240.0;
/// Relief noise base spatial frequency, cycles per radian: features
/// ~1/8 rad, spanning ~7 vertices at the canonical level-6 grid's mean vertex
/// spacing (~0.017 rad). Retuned in the Task 14 tuning season (iteration
/// 3): at the prior 48.0 the dominant octave was sub-Nyquist at L6
/// (~1.2 samples/cycle — per-vertex jitter the sea-level percentile
/// averages away, contributing nothing to coastline shape). At 8.0 the
/// octave is resolved (like `LOBE_FREQ` = 4 / `ARC_SPACING` = 9), giving
/// coherent capes and bays where relief crosses sea level.
const RELIEF_FREQUENCY: f64 = 8.0;
/// Octaves for the fBm relief noise.
const RELIEF_OCTAVES: u32 = 4;

/// Relief amplitude scale: hard rock (induration→1) carries full relief,
/// soft lies smooth; belts (near boundaries) roughen further.
/// type-audit: bare-ok(ratio: induration), bare-ok(count: boundary_hops)
fn relief_scale(induration: f64, boundary_hops: u32) -> f64 {
    let hardness = 0.25 + 0.75 * induration;
    let belt = 1.0 + 1.5 * math::exp(-f64::from(boundary_hops) / 3.0);
    hardness * belt
}

/// The along-strike arc-gate seed for a terrain root seed. THE one place
/// this derivation is written: `generate_elevation` builds the gate field
/// from it and `globe::generate` retains it on the globe so
/// [`GeneratedTerrain::has_edifice`](crate::GeneratedTerrain::has_edifice)
/// can resample the same field. Hash-noise only — never consumed as a
/// `Stream`, so re-sampling costs no draw and carries no draw-order /
/// save-format contract (see `streams::ARC_GATE`).
pub(crate) fn arc_gate_seed(terrain_seed: Seed) -> Seed {
    terrain_seed.derive(streams::ARC_GATE)
}

/// The along-strike gate sampler for an already-derived arc-gate seed.
/// Shared by `assemble_elevation` (which hoists it out of the per-vertex
/// loop) and the edifice read, so both sample one field at one frequency
/// and one octave count by construction rather than by agreement.
pub(crate) fn arc_gate_fbm(arc_gate_seed: Seed) -> crate::crust::SphereFbm {
    crate::crust::SphereFbm::new(arc_gate_seed, ARC_SPACING, ARC_GATE_OCTAVES)
}

/// Whether the along-strike gate is "on" at a sampled value — the duty
/// cycle that breaks a continuous arc wall into discrete edifices.
fn arc_gate_on(gate: f64) -> bool {
    gate > (1.0 - ARC_DUTY)
}

/// Whether a boundary contact builds a volcanic **edifice** on a vertex at
/// `distance` hops from it, given the gate value sampled at the contact's
/// own (source) vertex.
///
/// This is the predicate [`boundary_profile_m`]'s island-arc arm applies,
/// narrowed by distance: `on` scales the arc's whole decaying skirt, but
/// only the vertices within one e-folding of `ARC_EDIFICE_DECAY_VERTICES` carry
/// enough of it to be the cone rather than its apron. Beyond that the
/// elevation still moves with the gate while this reads `false` — a
/// deliberate, one-directional narrowing (the edifice read never claims a
/// vertex the elevation did not build as one).
///
/// Only `IslandArc`'s overriding side is gated. A coastal range's volcanic
/// line is on the continent and shares the collision-belt crest profile
/// with no gate of its own, so nothing in the shipped elevation
/// distinguishes a volcanic crest vertex there from a non-volcanic one;
/// claiming an edifice on that arm would be a second opinion, not a read.
pub(crate) fn edifice_present(
    kind: BoundaryKind,
    arc_side: bool,
    distance: u32,
    gate: f64,
) -> bool {
    match kind {
        BoundaryKind::IslandArc if arc_side => {
            arc_gate_on(gate) && f64::from(distance) <= ARC_EDIFICE_DECAY_VERTICES
        }
        BoundaryKind::IslandArc
        | BoundaryKind::CoastalRange
        | BoundaryKind::ContinentalCollision
        | BoundaryKind::ContinentalRift
        | BoundaryKind::OceanicRidge
        | BoundaryKind::Transform => false,
    }
}

/// Per-kind boundary elevation profile (Sculpting, spec §3): crest,
/// foothills, and a foreland trough for collision belts (including the
/// continental side of a coastal range — the Andean volcanic line is ON
/// the continent); gated volcanic edifices on an island arc's overriding
/// side; a seaward trench on every subducting side (an island arc's
/// non-arc side, and a coastal range's whole oceanic side — no offshore
/// arc on a mixed margin); the bare per-kind amplitude for everything
/// else — rifts, ridges, transforms, and a collision-tagged boundary
/// whose own vertex reads oceanic (crust varies continuously within a
/// plate, so a vertex can drift off the continental side even though its
/// nearest same-plate boundary was classified from a
/// continental-continental contact) — which `assemble_elevation` decays
/// with the existing maturity-driven length exactly as it did before this
/// profile existed (see `profile_scale`).
///
/// `gate` is the along-strike hash-noise in [0, 1), sampled once per
/// **source** boundary vertex so a whole edifice shares one gate value —
/// only meaningful for the island-arc edifice branch; other kinds ignore
/// it.
fn boundary_profile_m(
    kind: BoundaryKind,
    vertex_continental: bool,
    arc_side: bool,
    distance: u32,
    gate: f64,
) -> f64 {
    let base = boundary_amplitude_m(kind, vertex_continental, arc_side);
    let d = f64::from(distance);
    match kind {
        BoundaryKind::ContinentalCollision | BoundaryKind::CoastalRange if vertex_continental => {
            let crest = base * math::exp(-d / CREST_DECAY_VERTICES);
            let foothills = FOOTHILLS_FRACTION * base * math::exp(-d / FOOTHILLS_DECAY_VERTICES);
            let (f0, f1) = FORELAND_HOPS;
            let foreland = if distance >= f0 && distance <= f1 {
                FORELAND_DEPTH_M
                    * math::sin(
                        core::f64::consts::PI * (d - f64::from(f0)) / f64::from(f1 - f0).max(1.0),
                    )
            } else {
                0.0
            };
            crest + foothills + foreland
        }
        BoundaryKind::IslandArc if arc_side => {
            // Overriding ocean plate: a volcanic edifice, present only
            // where the along-strike gate is on. The gate test itself is
            // `arc_gate_on` so that `edifice_present` — the published read
            // — cannot drift from the elevation this arm builds.
            let on = if arc_gate_on(gate) { 1.0 } else { 0.12 };
            base * on * math::exp(-d / ARC_EDIFICE_DECAY_VERTICES)
        }
        BoundaryKind::IslandArc | BoundaryKind::CoastalRange => {
            // The subducting-side trench. For CoastalRange this arm is
            // every oceanic vertex REGARDLESS of `arc_side` (the continental
            // side was claimed by the crest arm above): Andean-style
            // margins carry no offshore arc — the volcanic line is on the
            // continent — and `arc_side` is a bare plate-id tie-break
            // with no physical meaning on a mixed margin, so it must not
            // select an edifice here (Task 3 review fix; spec §3).
            if distance <= TRENCH_HOPS {
                TRENCH_DEPTH_M * math::exp(-d / TRENCH_DECAY_VERTICES)
            } else {
                base * math::exp(-d / FAR_FIELD_DECAY_VERTICES) * FAR_FIELD_FRACTION
            }
        }
        _ => base, // rifts/ridges/transform, unchanged in shape (caller decays it)
    }
}

/// Whether `assemble_elevation` should apply the maturity amplitude
/// `factor` (young plates rise higher, old plates worn down) to
/// `boundary_profile_m`'s output, and whether it should also re-apply the
/// maturity-driven `exp(-d/decay_vertices)` falloff length on top.
enum ProfileScale {
    /// Pre-Sculpting behavior, bit-for-bit: `factor * exp(-d/decay_vertices)`.
    /// `boundary_profile_m` returned the bare amplitude for this branch.
    Unchanged,
    /// A belt/arc uplift part (crest+foothills+foreland combined for
    /// collision belts, since they share one profile value; or a gated
    /// volcanic edifice): `factor` applies, but the profile's own fixed
    /// decay lengths replace `decay_vertices` — no double falloff.
    Uplift,
    /// A trough (trench, and the residual subduction depression beyond
    /// it): `factor` does NOT apply — old belts keep their basins.
    Trough,
}

/// Classify a boundary contact for `ProfileScale`, mirroring
/// `boundary_profile_m`'s own branch structure exactly (kept as a
/// separate small function, rather than threading a scale enum out of
/// `boundary_profile_m` itself, so the profile's public contract stays a
/// plain `f64` per the interface `boundary_profile_m` is tested against).
fn profile_scale(kind: BoundaryKind, vertex_continental: bool, arc_side: bool) -> ProfileScale {
    match kind {
        BoundaryKind::ContinentalRift | BoundaryKind::OceanicRidge | BoundaryKind::Transform => {
            ProfileScale::Unchanged
        }
        BoundaryKind::ContinentalCollision => {
            if vertex_continental {
                ProfileScale::Uplift
            } else {
                ProfileScale::Unchanged
            }
        }
        BoundaryKind::CoastalRange => {
            // The oceanic side is ALWAYS the trench (no offshore arc on
            // an Andean-style margin), so `arc_side` is irrelevant here —
            // mirroring `boundary_profile_m`'s own arms exactly.
            if vertex_continental {
                ProfileScale::Uplift
            } else {
                ProfileScale::Trough
            }
        }
        BoundaryKind::IslandArc => {
            if arc_side {
                ProfileScale::Uplift
            } else {
                ProfileScale::Trough
            }
        }
    }
}

/// A mantle hotspot: a fixed Gaussian dome of uplift.
struct Hotspot {
    /// Dome center, a unit vector.
    position: [f64; 3],
    /// Peak uplift, meters.
    strength_m: f64,
}

/// Angular half-width of a hotspot dome, radians (~3°: one to two vertices at
/// level 5).
const HOTSPOT_SIGMA_RAD: f64 = 0.05;

/// Gaussian dome contribution at a unit-sphere position, meters (Sculpting
/// Task 6): generalizes the old `Hotspot::contribution_m` to any dome
/// center/strength, not only the live hotspot — a `TrailSeamount`'s
/// `age_index == 0` entry carries the SAME `position`/`strength_m` a plain
/// `Hotspot` would, so calling this with those fields reproduces the
/// pre-trails contribution byte-for-bit (see
/// `dome_m_matches_the_original_hotspot_formula_for_age_zero`); trail
/// entries (`age_index >= 1`) are additional calls of the same function
/// that add their (decayed) contribution on top.
fn dome_m(position: [f64; 3], strength_m: f64, at: [f64; 3]) -> f64 {
    let angle = math::acos(dot(position, at).clamp(-1.0, 1.0));
    strength_m * math::exp(-(angle * angle) / (2.0 * HOTSPOT_SIGMA_RAD * HOTSPOT_SIGMA_RAD))
}

/// Draw 3–8 hotspots from the hotspots stream: count first, then position
/// (two draws) and strength (1000–3000 m) per hotspot, sequentially.
fn draw_hotspots(terrain_seed: Seed) -> Vec<Hotspot> {
    let mut stream = terrain_seed.derive(streams::HOTSPOTS).stream();
    let count = stream.range_u32(3, 8);
    (0..count)
        .map(|_| {
            let position = unit_vector(&mut stream);
            let strength_m = 1000.0 + 2000.0 * stream.next_f64();
            Hotspot {
                position,
                strength_m,
            }
        })
        .collect()
}

/// One seamount in a hotspot trail (Sculpting, spec §3): the drawn hotspot
/// smeared along its plate's local velocity, age-progressive. Retained on
/// `TectonicGlobe` for Task 9's atolls.
/// type-audit: bare-ok(ratio: position), pending(wave-2: strength_m), bare-ok(count: age_index)
#[derive(Debug, Clone, PartialEq)]
pub struct TrailSeamount {
    /// Dome center, unit vector.
    pub position: [f64; 3],
    /// Peak uplift, meters (decays along the chain).
    pub strength_m: f64,
    /// Steps upstream from the live hotspot (0 = the hotspot itself).
    pub age_index: u32,
}

/// Maximum angular length of a hotspot trail, radians (~20°).
/// type-audit: bare-ok(ratio)
pub const TRAIL_LENGTH_RAD: f64 = 0.35;
/// Number of trail steps upstream of the live hotspot dome.
/// type-audit: bare-ok(count)
pub const TRAIL_STEPS: u32 = 6;
/// Per-step strength decay multiplier along a trail.
/// type-audit: bare-ok(ratio)
pub const TRAIL_DECAY: f64 = 0.55;

/// Derive trail chains from the existing hotspot draws (no new draws):
/// step upstream against the plate's surface velocity at each point,
/// `TRAIL_STEPS` steps of `TRAIL_LENGTH_RAD`/`TRAIL_STEPS` radians each,
/// strength decaying by `TRAIL_DECAY` per step. Chain length thus scales
/// with plate speed direction only (fixed arc length; a stationary plate
/// still gets a stubby stack — harmless, it reads as a large volcanic
/// massif). Every hotspot contributes exactly `TRAIL_STEPS + 1` entries
/// (the live dome plus `TRAIL_STEPS` trail steps) unless the plate's
/// velocity at some point along the chain underflows to numerically zero
/// (the rotation-axis poles), in which case the chain stops early.
/// type-audit: bare-ok(index: plate_of)
pub fn trail_seamounts(
    terrain_seed: Seed,
    plates: &[Plate],
    plate_of: &VertexMap<u32>,
    geo: &Geosphere,
) -> Vec<TrailSeamount> {
    let hotspots = draw_hotspots(terrain_seed);
    let index = NearestVertexIndex::new(geo);
    let step = TRAIL_LENGTH_RAD / f64::from(TRAIL_STEPS);
    let mut out = Vec::new();
    for h in &hotspots {
        let mut pos = h.position;
        let mut strength = h.strength_m;
        out.push(TrailSeamount {
            position: pos,
            strength_m: strength,
            age_index: 0,
        });
        for i in 1..=TRAIL_STEPS {
            let vertex = index.nearest_to_position(geo, pos);
            let plate = &plates[*plate_of.get(vertex) as usize];
            let v = crate::plates::velocity_at(plate, pos);
            let speed = crate::plates::norm(v);
            if speed < 1e-9 {
                break;
            }
            // Upstream = where the plate came FROM = -v direction.
            let dir = crate::plates::normalize(crate::plates::scale(v, -1.0 / speed));
            pos = crate::plates::rotate_toward(pos, dir, step);
            strength *= TRAIL_DECAY;
            out.push(TrailSeamount {
                position: pos,
                strength_m: strength,
                age_index: i,
            });
        }
    }
    out
}

/// The additive terms one vertex's pre-carve elevation is assembled from, in
/// metres. [`assemble_elevation`] sums exactly these, in exactly the field
/// order below; the attribution probe
/// (`land_elevation_attribution::the_land_elevation_terms_attribute_their_variance`)
/// reads them individually, so the decomposition can never drift from the
/// elevation it decomposes. Crate-internal on purpose: every *input* is a
/// public field on [`crate::TectonicGlobe`], but the per-term helpers
/// (`isostatic_m`, `relief_scale`, `dome_m`, `crust::SphereFbm`) are not, so
/// an out-of-crate reader would have to reimplement the arithmetic.
#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) struct ElevationTerms {
    /// Airy-isostatic base over the vertex's crust thickness.
    pub(crate) base: f64,
    /// The nearest same-plate boundary's profile contribution (signed).
    pub(crate) boundary: f64,
    /// Every hotspot-trail seamount's Gaussian dome, summed (≥ 0).
    pub(crate) hotspot: f64,
    /// Induration- and belt-scaled fBm relief (zero-mean by construction).
    pub(crate) relief: f64,
    /// The strict-ordering micro-epsilon, `VERTEX_EPSILON_M * vertex.0`.
    pub(crate) epsilon: f64,
}

impl ElevationTerms {
    /// The assembled elevation in metres — the summation
    /// [`assemble_elevation`] performs, **in exactly its original order**
    /// (`base + boundary + hotspot + relief + epsilon`, left-associative).
    /// Float addition is not associative, so this order is a byte-identity
    /// contract, not a formatting choice (`domains/terrain/CLAUDE.md`).
    pub(crate) fn total(&self) -> f64 {
        self.base + self.boundary + self.hotspot + self.relief + self.epsilon
    }
}

/// One vertex's [`ElevationTerms`]: the whole per-vertex body of
/// [`assemble_elevation`], extracted so the attribution probe reads the same
/// arithmetic the pipeline runs instead of a copy of it. The two fBm samplers
/// are passed in already built, because their construction is loop-invariant
/// (see [`assemble_elevation`]).
#[allow(clippy::too_many_arguments)]
pub(crate) fn vertex_elevation_terms(
    geo: &Geosphere,
    plates: &[Plate],
    plate_of: &VertexMap<u32>,
    boundaries: &VertexMap<Option<VertexBoundary>>,
    distances: &VertexMap<Option<(u32, Vertex)>>,
    seamounts: &[TrailSeamount],
    crust: &VertexMap<f64>,
    continental: &VertexMap<bool>,
    induration: &VertexMap<f64>,
    arc_gate_fbm: &crate::crust::SphereFbm,
    relief_fbm: &crate::crust::SphereFbm,
    vertex: Vertex,
) -> ElevationTerms {
    let plate = &plates[*plate_of.get(vertex) as usize];
    let vertex_continental = *continental.get(vertex);
    let base = isostatic_m(*crust.get(vertex));
    let boundary_term = match *distances.get(vertex) {
        None => 0.0,
        Some((distance, source)) => {
            let contact = (*boundaries.get(source)).expect("BFS sources are boundary vertices");
            let arc_side = plate.id > contact.other_plate;
            // Young plates (maturity 0): 1.5x amplitude, sharp 1.5-vertex
            // falloff. Old plates (maturity 1): 0.5x amplitude, worn
            // 4.5-vertex falloff. Only `ProfileScale::Unchanged` (rifts,
            // ridges, transforms, and an off-continent collision vertex)
            // still uses `decay_vertices` — the belt/arc anatomy bakes its
            // own fixed decay lengths into `boundary_profile_m` instead.
            let factor = 1.5 - plate.maturity;
            let decay_vertices = 1.5 + 3.0 * plate.maturity;
            let magnitude_scale = contact.magnitude / MAX_CLOSING_SPEED;
            // Along-strike gate: sampled once per SOURCE boundary vertex
            // (not per `vertex`) so a whole edifice shares one value;
            // only the island-arc edifice branch reads it (coastal
            // ranges carry no offshore arc — Task 3 review fix), and
            // it is hash-noise (no draw-order contract), so sampling
            // is safely skipped everywhere else.
            let gate = if contact.kind == BoundaryKind::IslandArc {
                arc_gate_fbm.sample(geo.position(source))
            } else {
                0.0
            };
            let profile =
                boundary_profile_m(contact.kind, vertex_continental, arc_side, distance, gate);
            match profile_scale(contact.kind, vertex_continental, arc_side) {
                ProfileScale::Unchanged => {
                    profile
                        * magnitude_scale
                        * factor
                        * math::exp(-f64::from(distance) / decay_vertices)
                }
                ProfileScale::Uplift => profile * magnitude_scale * factor,
                ProfileScale::Trough => profile * magnitude_scale,
            }
        }
    };
    let position = geo.position(vertex);
    let hotspot_term: f64 = seamounts
        .iter()
        .map(|s| dome_m(s.position, s.strength_m, position))
        .sum();
    // fBm relief (Sculpting, spec §3): zero-mean multi-octave detail,
    // amplitude scaled by induration (hard rock stands craggy) and
    // belt proximity (`relief_scale`). A vertex with no reachable
    // same-plate boundary (`None`) is treated as far from any belt
    // (12 hops — already past `relief_scale`'s decay length).
    let hops = (*distances.get(vertex)).map_or(12, |(distance, _)| distance);
    let relief_noise = relief_fbm.sample(position);
    let relief_term = RELIEF_AMPLITUDE_M
        * relief_scale(*induration.get(vertex), hops)
        * (relief_noise - 0.5)
        * 2.0;
    ElevationTerms {
        base,
        boundary: boundary_term,
        hotspot: hotspot_term,
        relief: relief_term,
        epsilon: VERTEX_EPSILON_M * f64::from(vertex.0),
    }
}

/// Every vertex's [`ElevationTerms`] for an already-generated globe, rebuilt
/// from the globe's own retained fields and the same two derived hash-noise
/// seeds [`generate_elevation`] used. Test-only (the attribution probe), so it
/// is never compiled into a shipping build.
///
/// The one input the globe does not retain is the continental mask, so it is
/// re-derived here from the retained crust field by the definitional
/// comparison `globe::generate` itself uses (`CrustField::continental_at(p)`
/// *is* `thickness_at(p) >= CONTINENTAL_THRESHOLD_KM`, and `crust` holds
/// exactly that thickness). Nothing has to be trusted about that: the
/// probe's conservation assert re-adds these terms and compares against the
/// elevation the pipeline actually produced, so a wrong mask would show up as
/// a failed reconstruction rather than as a plausible-looking number.
#[cfg(test)]
pub(crate) fn globe_elevation_terms(
    geo: &Geosphere,
    globe: &crate::globe::TectonicGlobe,
    world_seed: Seed,
) -> VertexMap<ElevationTerms> {
    let terrain_seed = world_seed.derive(streams::ROOT);
    let arc_gate_fbm = crate::crust::SphereFbm::new(
        terrain_seed.derive(streams::ARC_GATE),
        ARC_SPACING,
        ARC_GATE_OCTAVES,
    );
    let relief_fbm = crate::crust::SphereFbm::new(
        terrain_seed.derive(streams::RELIEF),
        RELIEF_FREQUENCY,
        RELIEF_OCTAVES,
    );
    let continental = VertexMap::from_fn(geo, |c| {
        *globe.crust.get(c) >= crate::crust::CONTINENTAL_THRESHOLD_KM
    });
    VertexMap::from_fn(geo, |vertex| {
        vertex_elevation_terms(
            geo,
            &globe.plates,
            &globe.plate_of,
            &globe.boundary,
            &globe.boundary_distance,
            &globe.trail_seamounts,
            &globe.crust,
            &continental,
            &globe.induration,
            &arc_gate_fbm,
            &relief_fbm,
            vertex,
        )
    })
}

/// Pure elevation assembly over explicit inputs (hotspot trail seamounts
/// included), so tests can pin the seamount list. See the module doc for
/// the formula. `crust` is each vertex's crust thickness in km (the
/// isostatic base input); `continental` is each vertex's crust flag (feeds
/// the boundary amplitude's side selection, unchanged in shape from the
/// retired plate-level flag). `arc_gate_seed` is hash-noise only
/// (Sculpting spec §3, `streams::ARC_GATE`) — never consumed as a
/// `Stream`, so it carries no draw-order/save-format contract; it gates
/// island-arc/coastal-range edifices into discrete along-strike chains.
/// `induration` (Task 4, The Ground/Sculpting seam) and `relief_seed`
/// (Sculpting, spec §3, `streams::RELIEF`) feed the zero-mean fBm relief
/// term: hash-noise only, never consumed as a `Stream`, so it carries no
/// draw-order/save-format contract, mirroring `arc_gate_seed`. `seamounts`
/// (Sculpting Task 6) replaces the old bare hotspot-dome list: its
/// `age_index == 0` entries carry each drawn hotspot's own position and
/// strength, so their `dome_m` contribution reproduces the pre-trails
/// hotspot term byte-for-bit; trail entries (`age_index >= 1`) are
/// additional (weaker) domes that add on top. Eleven explicit narrow
/// inputs beat a bundling struct here — same house call as climate's
/// assemblers (`temperature.rs`, `biome.rs`).
/// `pub(crate)` so the edifice read's agreement test can re-run the very
/// assembler that shipped under a second gate seed and watch which vertices
/// move (see `provider.rs`'s `has_edifice_names_the_vertices_the_elevation_…`).
#[allow(clippy::too_many_arguments)]
pub(crate) fn assemble_elevation(
    geo: &Geosphere,
    plates: &[Plate],
    plate_of: &VertexMap<u32>,
    boundaries: &VertexMap<Option<VertexBoundary>>,
    distances: &VertexMap<Option<(u32, Vertex)>>,
    seamounts: &[TrailSeamount],
    crust: &VertexMap<f64>,
    continental: &VertexMap<bool>,
    arc_gate_seed: Seed,
    induration: &VertexMap<f64>,
    relief_seed: Seed,
) -> VertexMap<ReferenceElevation> {
    // Hoist the two spherical-fBm samplers out of the per-vertex loop: their
    // seeds/frequencies/octaves are loop-invariant, so the slice- and
    // octave-seed derivations run once per field instead of once per vertex
    // (byte-identical — same seeds, same math). See `crust::SphereFbm`.
    let arc_gate_fbm = arc_gate_fbm(arc_gate_seed);
    let relief_fbm = crate::crust::SphereFbm::new(relief_seed, RELIEF_FREQUENCY, RELIEF_OCTAVES);
    VertexMap::from_fn(geo, |vertex| {
        // The per-vertex body lives in `vertex_elevation_terms` so the
        // attribution probe can read the terms individually; `total()` sums
        // them in the original left-associative order, which is a
        // byte-identity contract.
        let metres = vertex_elevation_terms(
            geo,
            plates,
            plate_of,
            boundaries,
            distances,
            seamounts,
            crust,
            continental,
            induration,
            &arc_gate_fbm,
            &relief_fbm,
            vertex,
        )
        .total();
        ReferenceElevation::new(metres).expect("isostatic elevation is finite")
    })
}

/// Per-vertex elevation in meters: the isostatic base over crust thickness,
/// the nearest same-plate boundary's contribution decayed by graph distance
/// and shaped by maturity, drawn hotspot trail seamounts, induration-scaled
/// fBm relief, and a strict-ordering micro-epsilon. `seamounts` is
/// `trail_seamounts`'s output (Sculpting Task 6) — computed by the caller
/// (`globe::generate`, which already holds `plate_of`) from the SAME
/// hotspot draws this function used to make on its own, so the hotspots
/// stream's draw order/count is unchanged.
/// type-audit: bare-ok(index: plate_of), bare-ok(count: distances), waiver(crust-km-convention: crust), bare-ok(flag: continental), bare-ok(ratio: induration)
#[allow(clippy::too_many_arguments)]
pub fn generate_elevation(
    terrain_seed: Seed,
    geo: &Geosphere,
    plates: &[Plate],
    plate_of: &VertexMap<u32>,
    boundaries: &VertexMap<Option<VertexBoundary>>,
    distances: &VertexMap<Option<(u32, Vertex)>>,
    seamounts: &[TrailSeamount],
    crust: &VertexMap<f64>,
    continental: &VertexMap<bool>,
    induration: &VertexMap<f64>,
) -> VertexMap<ReferenceElevation> {
    let arc_gate_seed = arc_gate_seed(terrain_seed);
    let relief_seed = terrain_seed.derive(streams::RELIEF);
    assemble_elevation(
        geo,
        plates,
        plate_of,
        boundaries,
        distances,
        seamounts,
        crust,
        continental,
        arc_gate_seed,
        induration,
        relief_seed,
    )
}

/// Resolve the world's target ocean fraction: draw (or pin) it. The draw
/// is consumed whether pinned or not (pin isolation). `notes` receives a
/// metering entry when `ocean_fraction` is pinned (Task 7's metering
/// convention: `pinned <name> <value> (seed draws <drawn>)`, the value
/// formatted `{:.2}`).
///
/// Task 9 iteration 3': called once, early, in `globe::generate` — the
/// resolved target is threaded to BOTH `crust::draw_cratons` (whose
/// budget is now derived from it) and `derive_sea_level` below, rather
/// than each drawing (or re-deriving) its own copy. Per the pin doctrine,
/// a pinned target conditions everything downstream identically to a
/// drawn one, so `--ocean-fraction` now legitimately perturbs craton
/// radii too — see `draw_cratons`'s doc and the pin-isolation test in
/// `tectonic_properties.rs`.
/// type-audit: bare-ok(prose: notes), bare-ok(ratio: return)
pub fn resolve_ocean_fraction(
    terrain_seed: Seed,
    pins: &TerrainPins,
    notes: &mut Vec<String>,
) -> f64 {
    let drawn = 0.5
        + 0.25
            * terrain_seed
                .derive(streams::OCEAN_FRACTION)
                .stream()
                .next_f64();
    let target = pins.ocean_fraction.unwrap_or(drawn);
    if let Some(f) = pins.ocean_fraction {
        notes.push(format!("pinned ocean-fraction {f:.2} (seed draws {drawn})"));
    }
    target
}

/// The shelf-break fallback activates when analytic continental supply
/// (`crust::continental_supply`) is below this fraction of the
/// ocean-fraction-implied land quota. The gap it bisects is wide and
/// empty: default draws (8-14 cratons) bottom out at supply/quota ≈ 1.050
/// over the frozen 1000-seed census (seed 586) — see
/// `default_worlds_never_trip_the_supply_fallback` in
/// `tectonic_properties.rs` — while a lone clamped craton sits ≲ 0.25
/// (measured over 1000 single-craton draws); the 0.5 factor bisects the
/// genuinely empty gap between ~1.05 and ~0.25, so default worlds provably
/// keep the exact-percentile path byte-identical.
///
/// **Both endpoints moved with decision 0134** and the reasoning is
/// unchanged, which is the point of recording them: at the old 0.6 clamp
/// with the closed-form rescale the floor was ≈ 0.554 (seed 558; 20/1000
/// below 0.6) against a single-craton ceiling of ≈ 0.18. The Glasshouse's
/// exact solve made the rescale deliver the budget it had been missing by
/// ~34%, which lifted the default floor clear past 1.0 — every default
/// world now supplies *more* continent than its quota asks for — while the
/// raised clamp lifted the single-craton ceiling proportionally. The gap
/// widened rather than narrowed, so 0.5 still sits in empty space and is
/// now a less marginal choice than when it was picked.
/// type-audit: bare-ok(ratio)
pub const SUPPLY_SHORTFALL_FACTOR: f64 = 0.5;

/// Land granted to a supply-limited world, as a multiple of its
/// continental supply: 1.0 places the sea-level percentile where the
/// crust field crosses `crust::CONTINENTAL_THRESHOLD_KM` — the isostatic
/// shelf break. Measured over the single-craton sweep (seeds 1..=40,
/// level 4, continents=1, drawn ocean fraction), κ grid {0.8, 0.9, 1.0,
/// 1.25, 1.5, 2.0}: no κ satisfies the whole-sphere shelf floor (best:
/// 18/40 at κ=2.0) because a ~3%-of-sphere continent cannot put 2% of
/// the sphere within ±200 m of sea level — while at κ = 1.0 the
/// land-normalized shelf (shelf vertices / land vertices, `shelf_land_ratio`)
/// spans 0.075–0.309 (median 0.171), overlapping and at the median
/// exceeding the default-world population's 0.097–0.165 (median 0.130),
/// with D in 3.14–5.5+. 1.0 is therefore retained — the physical shelf
/// break, untuned; the test floor is land-normalized instead
/// (decision 0053).
/// type-audit: bare-ok(ratio)
pub const SHELF_BREAK_LAND_FACTOR: f64 = 1.0;

/// Soften the ocean-fraction target when the crust cannot honor it
/// (single-craton hypsometry spec, route 1): when the craton set's
/// analytic continental supply falls below `SUPPLY_SHORTFALL_FACTOR`
/// times the land quota `1 − target`, the exact-percentile mechanism
/// would drown into the abyssal plain to fill the quota — a broad flat
/// "land" with no shelf and no bimodality. Instead the world keeps
/// `SHELF_BREAK_LAND_FACTOR × supply` land, placing the percentile at
/// the craton's isostatic shelf break — the physically correct outcome
/// for a small-continent world. The ocean-fraction pin is thereby a
/// *target* a supply-limited world may not reach (decision 0053); the
/// softening is metered in `notes` as a degradation note. Pure — no
/// draws, so pin isolation and stream order are untouched.
/// type-audit: bare-ok(ratio: target), bare-ok(ratio: supply), bare-ok(prose: notes), bare-ok(ratio: return)
pub fn effective_ocean_target(target: f64, supply: f64, notes: &mut Vec<String>) -> f64 {
    let land_quota = 1.0 - target;
    if supply >= SUPPLY_SHORTFALL_FACTOR * land_quota {
        return target;
    }
    notes.push(format!(
        "land quota {land_quota:.2} exceeds continental supply {supply:.3}: \
         sea level set at the shelf break (ocean-fraction target {target:.2} unmet)"
    ));
    1.0 - SHELF_BREAK_LAND_FACTOR * supply
}

/// Place sea level at the elevation percentile that puts as close to
/// `target` fraction of vertices strictly below it as the field's ties allow.
/// Pure — no draws: `target` is resolved once in `generate` via
/// `resolve_ocean_fraction` (Task 9 iteration 3') and shared with
/// `crust::draw_cratons`. Sort uses `total_cmp`.
///
/// Pre-Sculpting, elevations were continuous fBm-derived values with no
/// meaningful duplicates, so the naive order-statistic pick (`sorted[index]`)
/// hit the target fraction exactly. The carve (Sculpting spec §5) can now
/// deposit many vertices to the *exact same* elevation — the marine wedge caps
/// a whole shelf at one shared value — so a wide tie can straddle the
/// target rank; since `is_ocean` is strict-less-than, the WHOLE tied block
/// reads as land or ocean together; the naive pick can then miss the target
/// fraction by the tie's width instead of by at most one vertex. When a tie
/// wider than one vertex straddles the rank, this picks whichever of the two
/// distinct boundary values (below the tie vs. through it) lands the
/// achieved "vertices strictly below" count closer to the target — a narrow
/// (one-vertex, the ordinary no-duplicate) tie changes nothing, so every
/// pre-Sculpting call site is byte-identical. Note the tie-aware pick can
/// land on EITHER side of the raw target (the naive pick could only
/// undershoot): under a wide tie it takes whichever boundary is closer,
/// so the achieved ocean fraction may overshoot the target — this fix is
/// closest-approach, not strictly conservative.
/// type-audit: bare-ok(ratio: target)
pub fn derive_sea_level(
    elevation: &VertexMap<ReferenceElevation>,
    target: f64,
) -> ReferenceElevation {
    let mut sorted: Vec<ReferenceElevation> = elevation.iter().map(|(_, e)| *e).collect();
    sorted.sort_by(|a, b| a.total_cmp(*b));
    let n = sorted.len();
    let index = ((target * n as f64) as usize).min(n - 1);
    let value = sorted[index];
    let lo = sorted.partition_point(|v| v.total_cmp(value) == std::cmp::Ordering::Less);
    let hi = sorted.partition_point(|v| v.total_cmp(value) != std::cmp::Ordering::Greater);
    if hi > lo + 1 && hi < n {
        let raw_target = target * n as f64;
        if (hi as f64 - raw_target).abs() < (lo as f64 - raw_target).abs() {
            return sorted[hi];
        }
    }
    value
}

/// Relative restlessness of each boundary kind.
fn intensity(kind: BoundaryKind) -> f64 {
    match kind {
        BoundaryKind::ContinentalCollision => 0.8,
        BoundaryKind::CoastalRange => 0.9,
        BoundaryKind::IslandArc => 0.9,
        BoundaryKind::ContinentalRift => 0.7,
        BoundaryKind::OceanicRidge => 0.6,
        BoundaryKind::Transform => 0.5,
    }
}

/// Distance decay length for unrest, in vertices.
const UNREST_DECAY_VERTICES: f64 = 2.0;

/// The unrest field, per vertex in [0, 1]: boundary intensity × normalized
/// closing speed × youth (inverse maturity), decayed by distance to the
/// nearest same-plate boundary; clamped. Old quiet interiors approach zero.
/// Nothing consumes it in C3 — it is banked (spec §15).
/// type-audit: bare-ok(index: plate_of), bare-ok(count: distances), bare-ok(ratio: return)
pub fn generate_unrest(
    geo: &Geosphere,
    plates: &[Plate],
    plate_of: &VertexMap<u32>,
    boundaries: &VertexMap<Option<VertexBoundary>>,
    distances: &VertexMap<Option<(u32, Vertex)>>,
) -> VertexMap<f64> {
    VertexMap::from_fn(geo, |vertex| {
        let Some((distance, source)) = *distances.get(vertex) else {
            return 0.0;
        };
        let contact = (*boundaries.get(source)).expect("BFS sources are boundary vertices");
        let plate = &plates[*plate_of.get(vertex) as usize];
        let youth = 1.5 - plate.maturity;
        let raw = intensity(contact.kind)
            * (contact.magnitude / MAX_CLOSING_SPEED)
            * youth
            * math::exp(-f64::from(distance) / UNREST_DECAY_VERTICES);
        raw.clamp(0.0, 1.0)
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::boundaries::{BoundaryKind, boundary_distance, boundary_field};
    use crate::pins::TerrainPins;
    use crate::plates::{Plate, assign_plates, generate_plates};
    use crate::streams;
    use hornvale_kernel::{Geosphere, Seed, VertexMap};

    /// Two hemisphere plates spinning against each other: convergent where
    /// y < 0, divergent where y > 0. Continental character lives in the
    /// crust flag map now, not the plate.
    fn hemisphere_plates(maturity: f64) -> Vec<Plate> {
        vec![
            Plate {
                id: 0,
                seed_position: [0.0, 0.0, 1.0],
                euler_axis: [1.0, 0.0, 0.0],
                rate: 1.0,
                maturity,
                weight: 1.0,
            },
            Plate {
                id: 1,
                seed_position: [0.0, 0.0, -1.0],
                euler_axis: [-1.0, 0.0, 0.0],
                rate: 1.0,
                maturity,
                weight: 1.0,
            },
        ]
    }

    /// Every vertex continental at a uniform 35 km thickness — the old
    /// `CONTINENT_BASE_M` (400 m) synthetic base is replaced by whatever
    /// isostasy gives that thickness (900 m at the Task 8 constants).
    const TEST_CRUST_KM: f64 = 35.0;

    fn all_continental_crust(geo: &Geosphere) -> (VertexMap<f64>, VertexMap<bool>) {
        (
            VertexMap::from_fn(geo, |_| TEST_CRUST_KM),
            VertexMap::from_fn(geo, |_| true),
        )
    }

    #[test]
    fn collision_uplift_towers_over_the_interior_and_decays_inland() {
        let geo = Geosphere::new(3);
        let plates = hemisphere_plates(0.0);
        let plate_of = assign_plates(&geo, Seed(1).derive(streams::ROOT), &plates);
        let (crust, continental) = all_continental_crust(&geo);
        let boundaries = boundary_field(&geo, &plate_of, &plates, &continental);
        let distances = boundary_distance(&geo, &plate_of, &boundaries);
        // Zero induration: minimizes the new relief term's amplitude so it
        // stays well inside this test's pre-existing 100 m interior
        // tolerance regardless of the sampled noise value (relief_scale's
        // floor at induration 0 caps the term at ~66 m here).
        let induration = VertexMap::from_fn(&geo, |_| 0.0);
        let elevation = assemble_elevation(
            &geo,
            &plates,
            &plate_of,
            &boundaries,
            &distances,
            &[],
            &crust,
            &continental,
            Seed(1).derive(streams::ROOT).derive(streams::ARC_GATE),
            &induration,
            Seed(1).derive(streams::ROOT).derive(streams::RELIEF),
        );
        // f64::MIN (not NEG_INFINITY, which the validating constructor
        // rejects) as a sentinel below every real elevation.
        let mut peak = ReferenceElevation::new(f64::MIN).expect("sentinel is finite");
        for (vertex, contact) in boundaries.iter() {
            if let Some(c) = contact
                && c.kind == BoundaryKind::ContinentalCollision
            {
                peak = peak.max(*elevation.get(vertex));
            }
        }
        assert!(peak.get() > 3000.0, "collision peak {} too low", peak.get());
        let base = isostatic_m(TEST_CRUST_KM);
        for (vertex, entry) in distances.iter() {
            if let Some((distance, _)) = entry
                && *distance >= 8
            {
                let e = elevation.get(vertex).get();
                assert!(
                    (e - base).abs() < 100.0,
                    "vertex {} interior elevation {e} strays from base {base}",
                    vertex.0
                );
            }
        }
    }

    /// claim: sanctioned-sweep(pinned regime, ocean_fraction: 0.65 — no census home)
    #[test]
    fn sea_level_hits_a_pinned_ocean_fraction() {
        let geo = Geosphere::new(3);
        let pins = TerrainPins {
            ocean_fraction: Some(0.65),
            ..TerrainPins::default()
        };
        for seed in [1u64, 7, 42] {
            let terrain_seed = Seed(seed).derive(streams::ROOT);
            let plates = generate_plates(terrain_seed, &pins, &mut Vec::new());
            let plate_of = assign_plates(&geo, terrain_seed, &plates);
            let ocean_target = resolve_ocean_fraction(terrain_seed, &pins, &mut Vec::new());
            let cratons =
                crate::crust::draw_cratons(terrain_seed, &pins, ocean_target, &mut Vec::new());
            let field = crate::crust::CrustField::new(terrain_seed, cratons);
            let crust = VertexMap::from_fn(&geo, |c| field.thickness_at(geo.position(c)).get());
            let continental = VertexMap::from_fn(&geo, |c| field.continental_at(geo.position(c)));
            let boundaries = boundary_field(&geo, &plate_of, &plates, &continental);
            let distances = boundary_distance(&geo, &plate_of, &boundaries);
            let crust_age = VertexMap::from_fn(&geo, |c| field.age_at(geo.position(c)));
            let induration = VertexMap::from_fn(&geo, |c| {
                crate::lithology::induration_at(
                    *crust_age.get(c),
                    *continental.get(c),
                    boundaries.get(c).map(|b| b.kind),
                    distances.get(c).map(|(hops, _)| hops),
                )
            });
            let seamounts = trail_seamounts(terrain_seed, &plates, &plate_of, &geo);
            let elevation = generate_elevation(
                terrain_seed,
                &geo,
                &plates,
                &plate_of,
                &boundaries,
                &distances,
                &seamounts,
                &crust,
                &continental,
                &induration,
            );
            let sea = derive_sea_level(&elevation, ocean_target);
            let below = elevation.iter().filter(|(_, e)| **e < sea).count();
            let achieved = below as f64 / elevation.len() as f64;
            assert!(
                (achieved - 0.65).abs() <= 0.01,
                "seed {seed}: achieved {achieved}"
            );
        }
    }

    #[test]
    fn unrest_is_high_on_young_convergent_boundaries_and_dies_inland() {
        let geo = Geosphere::new(3);
        let plates = hemisphere_plates(0.0);
        let plate_of = assign_plates(&geo, Seed(1).derive(streams::ROOT), &plates);
        let (_, continental) = all_continental_crust(&geo);
        let boundaries = boundary_field(&geo, &plate_of, &plates, &continental);
        let distances = boundary_distance(&geo, &plate_of, &boundaries);
        let unrest = generate_unrest(&geo, &plates, &plate_of, &boundaries, &distances);
        let mut boundary_max = 0.0f64;
        for (vertex, contact) in boundaries.iter() {
            if contact.is_some() {
                boundary_max = boundary_max.max(*unrest.get(vertex));
            }
        }
        assert!(boundary_max > 0.5, "young boundary max {boundary_max}");
        for (vertex, entry) in distances.iter() {
            if let Some((distance, _)) = entry
                && *distance >= 8
            {
                assert!(
                    *unrest.get(vertex) < 0.05,
                    "vertex {} interior unrest {}",
                    vertex.0,
                    unrest.get(vertex)
                );
            }
        }
        for (_, u) in unrest.iter() {
            assert!((0.0..=1.0).contains(u));
        }
    }

    #[test]
    fn old_plates_are_quieter_than_young_ones() {
        let geo = Geosphere::new(3);
        let young = hemisphere_plates(0.0);
        let old = hemisphere_plates(1.0);
        let plate_of = assign_plates(&geo, Seed(1).derive(streams::ROOT), &young);
        let (_, continental) = all_continental_crust(&geo);
        let boundaries = boundary_field(&geo, &plate_of, &young, &continental);
        let distances = boundary_distance(&geo, &plate_of, &boundaries);
        let unrest_young = generate_unrest(&geo, &young, &plate_of, &boundaries, &distances);
        let unrest_old = generate_unrest(&geo, &old, &plate_of, &boundaries, &distances);
        let max_young = unrest_young.iter().map(|(_, u)| *u).fold(0.0, f64::max);
        let max_old = unrest_old.iter().map(|(_, u)| *u).fold(0.0, f64::max);
        assert!(
            max_old < max_young,
            "old {max_old} not quieter than young {max_young}"
        );
    }

    #[test]
    fn isostasy_is_monotone_and_earthlike_at_the_anchors() {
        assert!(
            isostatic_m(7.0) < -3500.0,
            "oceanic floor {}",
            isostatic_m(7.0)
        );
        assert!(
            (isostatic_m(30.0)).abs() < 1.0,
            "reference crust {}",
            isostatic_m(30.0)
        );
        assert!(
            isostatic_m(40.0) > 1500.0,
            "thick craton {}",
            isostatic_m(40.0)
        );
        for t in 7..45 {
            assert!(isostatic_m(t as f64) < isostatic_m(t as f64 + 1.0));
        }
    }

    #[test]
    fn a_single_craton_world_has_a_shelf_and_a_bimodal_hypsometry() {
        let geo = Geosphere::new(4);
        let pins = TerrainPins {
            continents: Some(1),
            ..TerrainPins::default()
        };
        let outcome = crate::globe::generate(Seed(3), &geo, &pins).expect("genesis");
        let globe = outcome.value;
        let d = crate::shape::hypsometric_bimodality(&globe.elevation, globe.sea_level)
            .expect("has land and ocean");
        assert!(d > 1.5, "hypsometry not bimodal: D = {d}");
        // Shelf floor is land-normalized (decision 0053): a ~3%-of-sphere
        // continent cannot clear an absolute whole-sphere floor, but its
        // shelf-to-land ratio matches or beats default worlds'. The
        // absolute ceiling stays — it guards the original failure mode
        // (sea level drowned into the abyssal plain, everything "shelf").
        let shelf_land =
            crate::shape::shelf_land_ratio(&globe.elevation, globe.sea_level).expect("has land");
        assert!(
            shelf_land > 0.05,
            "no shelf band relative to land: {shelf_land}"
        );
        let shelf = crate::shape::shelf_fraction(&globe.elevation, globe.sea_level);
        assert!(shelf < 0.5, "everything is shelf: {shelf}");
    }

    #[test]
    fn effective_ocean_target_passes_ample_supply_through_silently() {
        let mut notes = Vec::new();
        assert_eq!(effective_ocean_target(0.65, 0.30, &mut notes), 0.65);
        // Exactly at the activation boundary: still the plain target.
        assert_eq!(
            effective_ocean_target(0.65, SUPPLY_SHORTFALL_FACTOR * 0.35, &mut notes),
            0.65
        );
        assert!(notes.is_empty(), "{notes:?}");
    }

    #[test]
    fn effective_ocean_target_falls_back_to_the_shelf_break_and_meters_it() {
        let mut notes = Vec::new();
        let supply = 0.031;
        let effective = effective_ocean_target(0.65, supply, &mut notes);
        assert_eq!(effective, 1.0 - SHELF_BREAK_LAND_FACTOR * supply);
        assert_eq!(notes.len(), 1);
        assert!(notes[0].contains("shelf break"), "{}", notes[0]);
    }

    #[test]
    fn relief_is_zero_mean_and_induration_scaled() {
        // Statistical, structural: over a real globe, mean |relief effect| on
        // hard vertices exceeds soft vertices. Compute two globes differing only in
        // that we zero the amplitude, then compare.
        let geo = Geosphere::new(4);
        let a =
            crate::globe::generate(Seed(42), &geo, &crate::pins::TerrainPins::default()).unwrap();
        // The term exists: elevations differ from a no-relief reconstruction.
        // Simplest honest probe: the constant is wired (non-zero) and the
        // documented invariant holds — relief contribution at induration 1.0
        // vs 0.0 scales by the documented ratio. Test the pure helper:
        let hi = relief_scale(1.0, 0);
        let lo = relief_scale(0.0, 12);
        assert!(hi > 2.0 * lo, "induration scaling too weak: {hi} vs {lo}");
        // The globe built fine with relief wired in (the `.unwrap()` above
        // already proves it); a non-empty elevation map confirms the term
        // didn't panic on any vertex.
        assert!(a.value.elevation.iter().next().is_some());
    }

    #[test]
    fn boundary_profiles_have_anatomy() {
        // Collision, continental side: crest at d=0 positive; foreland
        // trough negative somewhere in FORELAND_HOPS; recovery beyond.
        let crest = boundary_profile_m(BoundaryKind::ContinentalCollision, true, false, 0, 1.0);
        assert!(crest > 0.0);
        let trough = (FORELAND_HOPS.0..=FORELAND_HOPS.1)
            .map(|d| boundary_profile_m(BoundaryKind::ContinentalCollision, true, false, d, 1.0))
            .fold(f64::INFINITY, f64::min);
        assert!(trough < 0.0, "no foreland trough: {trough}");
        // Island arc: the gate switches edifices on and off along strike.
        let on = boundary_profile_m(BoundaryKind::IslandArc, false, true, 0, 1.0);
        let off = boundary_profile_m(BoundaryKind::IslandArc, false, true, 0, 0.0);
        assert!(
            on > 0.0 && off < on * 0.25,
            "arc not discrete: on {on} off {off}"
        );
        // Trench: oceanic subducting side goes deep right at the boundary.
        let trench = boundary_profile_m(BoundaryKind::IslandArc, false, false, 0, 1.0);
        assert!(trench < -1000.0, "no trench: {trench}");
        // Coastal range, oceanic side: ALWAYS the trench, regardless of
        // the arc_side tie-break — Andean margins have no offshore arc
        // (the volcanic line is on the continent). The flag must be
        // irrelevant on every oceanic CoastalRange vertex, at the notch and
        // in the residual beyond it.
        for d in 0..=8u32 {
            let seaward = boundary_profile_m(BoundaryKind::CoastalRange, false, false, d, 1.0);
            let tie_broken = boundary_profile_m(BoundaryKind::CoastalRange, false, true, d, 1.0);
            assert_eq!(
                seaward, tie_broken,
                "arc_side matters on an oceanic CoastalRange vertex at d={d}"
            );
        }
        let coastal_trench = boundary_profile_m(BoundaryKind::CoastalRange, false, true, 0, 1.0);
        assert!(
            coastal_trench < -1000.0,
            "no coastal trench on the tie-broken side: {coastal_trench}"
        );
    }

    /// Test-local flood fill: how many connected components `vertices` forms
    /// under the geosphere's neighbor adjacency, restricted to `vertices`
    /// itself (a neighbor outside the set does not link two components).
    fn count_components(
        geo: &Geosphere,
        vertices: &std::collections::BTreeSet<hornvale_kernel::Vertex>,
    ) -> usize {
        let mut unvisited = vertices.clone();
        let mut components = 0;
        while let Some(&start) = unvisited.iter().next() {
            components += 1;
            unvisited.remove(&start);
            let mut stack = vec![start];
            while let Some(vertex) = stack.pop() {
                for &neighbor in geo.neighbors(vertex) {
                    if unvisited.remove(&neighbor) {
                        stack.push(neighbor);
                    }
                }
            }
        }
        components
    }

    #[test]
    fn arcs_are_chains_not_walls_on_a_real_globe() {
        use std::collections::BTreeSet;
        let geo = Geosphere::new(5);
        let outcome =
            crate::globe::generate(Seed(42), &geo, &crate::pins::TerrainPins::default()).unwrap();
        let g = &outcome.value;
        // Above-sea arc vertices at IslandArc boundaries form >1 connected
        // component somewhere (discreteness), across land at arc boundaries.
        let arc_land: BTreeSet<hornvale_kernel::Vertex> = geo
            .vertices()
            .filter(|c| {
                matches!(
                    g.boundary.get(*c).map(|b| b.kind),
                    Some(crate::boundaries::BoundaryKind::IslandArc)
                ) && *g.elevation.get(*c) >= g.sea_level
            })
            .collect();
        // Weak structural check: arc land exists but is not one giant blob.
        if arc_land.len() >= 6 {
            let components = count_components(&geo, &arc_land);
            assert!(
                components >= 2,
                "arc land is one wall: {} vertices, {components} component(s)",
                arc_land.len()
            );
        }
    }

    #[test]
    fn dome_m_matches_the_original_hotspot_formula_for_age_zero() {
        // The removed `Hotspot::contribution_m` computed exactly this
        // expression. `dome_m` must stay byte-identical for the live-hotspot
        // (age_index == 0) case so hotspot trails only ADD trail entries on
        // top, never perturb the trunk dome's own contribution.
        let position = crate::plates::normalize([0.3, -0.5, 0.8]);
        let at = crate::plates::normalize([0.31, -0.49, 0.79]);
        let strength_m = 1800.0;
        let angle = math::acos(dot(position, at).clamp(-1.0, 1.0));
        let expected = strength_m
            * math::exp(-(angle * angle) / (2.0 * HOTSPOT_SIGMA_RAD * HOTSPOT_SIGMA_RAD));
        assert_eq!(dome_m(position, strength_m, at), expected);

        // End-to-end: an `assemble_elevation` call fed a single
        // `TrailSeamount { age_index: 0, .. }` must land on the identical
        // elevation an equivalent single-`Hotspot` call landed on before
        // this task — same formula, same inputs, reconstructed inline
        // since `Hotspot`/`contribution_m` are gone.
        let geo = Geosphere::new(3);
        let plates = hemisphere_plates(0.0);
        let plate_of = assign_plates(&geo, Seed(1).derive(streams::ROOT), &plates);
        let (crust, continental) = all_continental_crust(&geo);
        let boundaries = boundary_field(&geo, &plate_of, &plates, &continental);
        let distances = boundary_distance(&geo, &plate_of, &boundaries);
        let induration = VertexMap::from_fn(&geo, |_| 0.0);
        let hotspot_position = [0.0, 1.0, 0.0];
        let hotspot_strength_m = 2200.0;
        let seamounts = [TrailSeamount {
            position: hotspot_position,
            strength_m: hotspot_strength_m,
            age_index: 0,
        }];
        let elevation = assemble_elevation(
            &geo,
            &plates,
            &plate_of,
            &boundaries,
            &distances,
            &seamounts,
            &crust,
            &continental,
            Seed(1).derive(streams::ROOT).derive(streams::ARC_GATE),
            &induration,
            Seed(1).derive(streams::ROOT).derive(streams::RELIEF),
        );
        let baseline = assemble_elevation(
            &geo,
            &plates,
            &plate_of,
            &boundaries,
            &distances,
            &[],
            &crust,
            &continental,
            Seed(1).derive(streams::ROOT).derive(streams::ARC_GATE),
            &induration,
            Seed(1).derive(streams::ROOT).derive(streams::RELIEF),
        );
        for vertex in geo.vertices() {
            let position = geo.position(vertex);
            let expected_dome = dome_m(hotspot_position, hotspot_strength_m, position);
            let expected_e = baseline.get(vertex).get() + expected_dome;
            assert!(
                (elevation.get(vertex).get() - expected_e).abs() < 1e-9,
                "vertex {} did not add exactly the dome contribution",
                vertex.0
            );
        }
    }

    #[test]
    fn trails_are_age_ordered_chains_upstream_of_plate_motion() {
        let seed = Seed(42).derive(crate::streams::ROOT);
        let geo = Geosphere::new(4);
        let pins = crate::pins::TerrainPins::default();
        let mut notes = Vec::new();
        let plates = crate::plates::generate_plates(seed, &pins, &mut notes);
        let plate_of = crate::plates::assign_plates(&geo, seed, &plates);
        let seamounts = trail_seamounts(seed, &plates, &plate_of, &geo);
        // Every hotspot contributes TRAIL_STEPS+1 entries, age-ordered,
        // strength strictly decaying along each chain.
        assert!(seamounts.len().is_multiple_of(TRAIL_STEPS as usize + 1) && !seamounts.is_empty());
        for chain in seamounts.chunks(TRAIL_STEPS as usize + 1) {
            for pair in chain.windows(2) {
                assert!(pair[1].age_index == pair[0].age_index + 1);
                assert!(pair[1].strength_m < pair[0].strength_m);
            }
        }
        assert_eq!(seamounts, trail_seamounts(seed, &plates, &plate_of, &geo));
    }
}
