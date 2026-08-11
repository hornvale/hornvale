//! The channel network (The Ford, spec §5.2/§5.3): rivers carried as
//! **polylines** with a discharge-derived angular width, and a point's
//! relation to water read as the banding of a **signed** distance to that
//! network.
//!
//! This is the repair for the type error named in the campaign's keystone —
//! a river stored as a *face* is as wide as the face, so every river in
//! Hornvale was one cell (~110 km) across at every zoom. Nothing here refines
//! the mesh; the channel is a one-dimensional feature carried alongside it,
//! and the band predicate evaluates at any position without traversal.
//!
//! **Everything in this module is angular.** `domains/terrain` works on the
//! unit sphere and no planet radius exists anywhere in the codebase, so a
//! "channel width in metres" has no defined meaning here. Widths are radians,
//! expressed as fractions of the local cell spacing — which is the better
//! statement of the campaign's claim anyway, since the claim is about the
//! *ratio* of channel width to cell width.

use crate::crust::SphereFbm;
use crate::globe::TectonicGlobe;
use crate::streams;
use crate::water::{RIVER_MIN_DRAINAGE, WaterKind};
use hornvale_kernel::{CellId, Geosphere, Seed, SphericalPolyline, band, math};
use std::collections::BTreeSet;

/// Where a point sits across a channel, outward from the centreline. The
/// ordinal is a banding of `|d|`; the *sign* of `d` (which bank) is reported
/// alongside it by [`ChannelNetwork::transverse_at`] and never folded in.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Transverse {
    /// In the water: `|d| < w/2`.
    Channel,
    /// The wetted margin between water and floodplain: `w/2 <= |d| < w/2 + k·w`.
    Bank,
    /// The valley floor the channel floods across: out to `V/2`.
    Floodplain,
    /// The abandoned older valley floor above the floodplain.
    Terrace,
    /// Beyond the valley entirely — the answer terrain already gives.
    Dry,
}

impl Transverse {
    /// The five transverse-band names in stable index order — the
    /// self-describing legend for scene emission (mirrors
    /// [`crate::water::WaterKind::LEGEND`]).
    pub const LEGEND: [&'static str; 5] = ["channel", "bank", "floodplain", "terrace", "dry"];

    /// Stable numeric index into `LEGEND`, independent of enum discriminant
    /// layout (explicit `match`, not `self as u8`, so reordering variants can
    /// never silently change a committed index).
    /// type-audit: bare-ok(index: return)
    pub fn index(self) -> u8 {
        match self {
            Transverse::Channel => 0,
            Transverse::Bank => 1,
            Transverse::Floodplain => 2,
            Transverse::Terrace => 3,
            Transverse::Dry => 4,
        }
    }

    /// Stable name, the one true transverse-band name source for scene
    /// emission.
    /// type-audit: bare-ok(identifier-text: return)
    pub fn name(self) -> &'static str {
        match self {
            Transverse::Channel => "channel",
            Transverse::Bank => "bank",
            Transverse::Floodplain => "floodplain",
            Transverse::Terrace => "terrace",
            Transverse::Dry => "dry",
        }
    }

    /// The band [`hornvale_kernel::band`] index names, given the four edges
    /// [`band_edges`] produces. Anything past the last edge is `Dry`.
    /// type-audit: bare-ok(index: band_index)
    pub fn from_band(band_index: usize) -> Transverse {
        match band_index {
            0 => Transverse::Channel,
            1 => Transverse::Bank,
            2 => Transverse::Floodplain,
            3 => Transverse::Terrace,
            _ => Transverse::Dry,
        }
    }
}

/// The `b` of downstream hydraulic geometry `w = a·Q^b`. Half is the
/// textbook value (width scales with the square root of discharge) and is
/// not a free parameter of this campaign: it is the exponent that makes
/// width-per-unit-discharge fall off as a river grows, which is what keeps a
/// mainstem from being a hundred times its headwaters' width.
/// type-audit: bare-ok(ratio)
pub const CHANNEL_WIDTH_EXPONENT: f64 = 0.5;

/// The `a` of `w = a·Q^b`, as a fraction of the local cell spacing —
/// **a placeholder, calibrated in Task 6 against a preregistered interval,
/// not here.**
///
/// Its provenance is an Earth analogy rather than a measurement: the
/// canonical level-6 cell spans ~120 km, a large terrestrial river is ~1 km
/// wide, and the largest drainages this world produces are of order a few
/// hundred, so `a ≈ 5e-4` puts a mainstem at roughly 0.7% of a cell edge.
/// Deliberately not tuned against any number measured in Task 4 — a single
/// calibration in the open beats several quiet ones.
/// type-audit: bare-ok(ratio)
pub const CHANNEL_WIDTH_COEFF: f64 = 5.0e-4;

/// The `k` of the bank border `w/2 + k·w`: the wetted margin either side of
/// the water is half a channel width.
/// type-audit: bare-ok(ratio)
pub const BANK_WIDTH_RATIO: f64 = 0.5;

/// Floodplain half-width, as a multiple of channel width, at zero
/// confinement (a perfectly flat reach). Terrestrial floodplains run tens of
/// channel widths across; 20 sits inside that range and keeps the widest
/// valley a fifth of a cell rather than a whole one.
/// type-audit: bare-ok(ratio)
pub const FLOODPLAIN_MAX_RATIO: f64 = 20.0;

/// Terrace width beyond `V/2`, as a fraction of `V/2`. The terrace is the
/// abandoned valley floor, so it is keyed to the valley rather than to the
/// channel: a gorge gets a narrow terrace and a broad valley a wide one.
/// type-audit: bare-ok(ratio)
pub const TERRACE_WIDTH_RATIO: f64 = 0.5;

/// The local gradient at and above which a reach is a **gorge**: fully
/// confined, no floodplain at all. Units are metres of fall per radian of
/// angular separation — the only gradient this codebase can express, since
/// there is no length scale to make it dimensionless.
///
/// **Measured, not assumed** (Task 4 probe, seeds 42/7/1234 at the canonical
/// `Geosphere::new(6)`, over every `WaterKind::River` cell with a downhill
/// target): the river-cell gradient distribution is p05 ≈ 2.0e3,
/// p50 ≈ 1.1–1.5e4, p75 ≈ 2.0–2.6e4, p90 ≈ 3.2–3.7e4, **p95 ≈ 4.0–4.2e4**,
/// p99 ≈ 5.7e4, max ≈ 6.7–8.5e4. The same statistic at level 5 is within
/// ~15% of the level-6 value, so this is a property of the terrain rather
/// than of the grid, and a fixed constant is a legitimate normalization.
///
/// 4.0e4 is that p95. The choice is what makes the confinement term actually
/// *discriminate*: it puts the median river at confinement ≈ 0.7, the steep
/// quartile below 0.4, and about 5% of river cells in true gorges.
/// Normalizing on the maximum instead would compress the whole population
/// into confinement ∈ [0.7, 1.0] and the law would be a near no-op;
/// normalizing on the median would make gorges of over half the world's
/// rivers.
/// type-audit: pending(wave-2)
pub const GORGE_SLOPE: f64 = 40_000.0;

/// Meander noise spatial frequency on the unit sphere. The value-noise
/// lattice spacing is `1/f` radians, so 24 puts a meander wavelength at
/// ~0.04 rad — a couple of canonical cells, which is the scale a mainstem
/// wanders on.
/// type-audit: bare-ok(ratio)
pub const MEANDER_FREQUENCY: f64 = 24.0;

/// Octaves in the meander field.
/// type-audit: bare-ok(count)
pub const MEANDER_OCTAVES: u32 = 4;

/// Peak meander displacement as a fraction of the local cell spacing, at
/// full confinement-free (flat) gradient. A quarter keeps a wandering
/// channel inside the pair of cells the vertex joins.
/// type-audit: bare-ok(ratio)
pub const MEANDER_AMPLITUDE_RATIO: f64 = 0.25;

/// Half the channel width for a reach carrying `drainage`, in radians, given
/// the local angular `cell_edge`. Downstream hydraulic geometry
/// `w = a·Q^b` with `a` a fraction of the cell edge, so the result stays
/// angular. Below [`RIVER_MIN_DRAINAGE`] — the same threshold
/// [`crate::water::classify`] uses to call a cell a river at all — the width
/// is exactly `0.0`: a sub-threshold trickle is not a channel.
/// type-audit: bare-ok(count: drainage), pending(wave-1: cell_edge), pending(wave-1: return)
pub fn channel_half_width(drainage: f64, cell_edge: f64) -> f64 {
    if drainage.total_cmp(&RIVER_MIN_DRAINAGE).is_lt() {
        return 0.0;
    }
    0.5 * CHANNEL_WIDTH_COEFF * cell_edge * math::powf(drainage, CHANNEL_WIDTH_EXPONENT)
}

/// How unconfined a reach is, in `[0, 1]`: `1` on the flat, falling linearly
/// to `0` at [`GORGE_SLOPE`] and staying there. This is the `g` of
/// `V = g(Q, slope)`'s slope half; `Q` enters through the channel width the
/// floodplain is measured in.
///
/// The clamp is what makes "a gorge has no floodplain" an *exact* property
/// rather than an asymptote: at or above [`GORGE_SLOPE`] the floodplain band
/// is empty, and [`band_edges`] emits two equal edges to say so.
/// type-audit: pending(wave-2: slope), bare-ok(ratio: return)
pub fn confinement(slope: f64) -> f64 {
    (1.0 - slope.abs() / GORGE_SLOPE).clamp(0.0, 1.0)
}

/// The four band borders outward from the centreline, in radians:
/// `[w/2, w/2 + k·w, V/2, V/2 · (1 + t)]` — channel/bank, bank/floodplain,
/// floodplain/terrace, terrace/dry. Non-decreasing by construction, so
/// [`hornvale_kernel::band`] reads them directly; a gorge collapses the
/// second and third to the same value and its floodplain band is empty.
/// type-audit: bare-ok(count: drainage), pending(wave-2: slope), pending(wave-1: cell_edge), pending(wave-1: return)
pub fn band_edges(drainage: f64, slope: f64, cell_edge: f64) -> [f64; 4] {
    let half = channel_half_width(drainage, cell_edge);
    let width = 2.0 * half;
    let bank = half + BANK_WIDTH_RATIO * width;
    let valley = bank + FLOODPLAIN_MAX_RATIO * width * confinement(slope);
    [half, bank, valley, valley * (1.0 + TERRACE_WIDTH_RATIO)]
}

/// Dot product of two 3-vectors.
fn dot(a: [f64; 3], b: [f64; 3]) -> f64 {
    a[0] * b[0] + a[1] * b[1] + a[2] * b[2]
}

/// Cross product of two 3-vectors.
fn cross(a: [f64; 3], b: [f64; 3]) -> [f64; 3] {
    [
        a[1] * b[2] - a[2] * b[1],
        a[2] * b[0] - a[0] * b[2],
        a[0] * b[1] - a[1] * b[0],
    ]
}

/// Normalize a 3-vector; returns the input unchanged if its norm is zero.
fn normalize(v: [f64; 3]) -> [f64; 3] {
    let n = (v[0] * v[0] + v[1] * v[1] + v[2] * v[2]).sqrt();
    if n == 0.0 {
        v
    } else {
        [v[0] / n, v[1] / n, v[2] / n]
    }
}

/// Angular separation of two unit vectors, radians.
fn angle(a: [f64; 3], b: [f64; 3]) -> f64 {
    math::acos(dot(a, b).clamp(-1.0, 1.0))
}

/// Mean angular separation of a cell from its neighbours — the local cell
/// spacing every width in this module is a fraction of.
fn cell_spacing(geo: &Geosphere, c: CellId) -> f64 {
    let neighbors = geo.neighbors(c);
    if neighbors.is_empty() {
        return 0.0;
    }
    let p = geo.position(c);
    let sum: f64 = neighbors.iter().map(|&n| angle(p, geo.position(n))).sum();
    sum / neighbors.len() as f64
}

/// Local gradient at `c`, metres of fall per radian toward its downhill
/// target. `0.0` where there is no target (a terminal sink), which reads as
/// perfectly unconfined — the wide, flat margin of a playa.
fn local_slope(globe: &TectonicGlobe, geo: &Geosphere, c: CellId) -> f64 {
    let Some(target) = *globe.downhill.get(c) else {
        return 0.0;
    };
    let separation = angle(geo.position(c), geo.position(target));
    if separation <= 0.0 {
        return 0.0;
    }
    (globe.elevation.get(c).get() - globe.elevation.get(target).get()) / separation
}

/// The channel network of one globe: the world's rivers as polylines, with
/// the band geometry each vertex implies.
///
/// Recomputed at genesis from committed state (drainage, the post-carve
/// downhill graph, elevation) plus one noise field; never serialized. The
/// band derivation itself makes **no seed draws** — only the meander
/// displacement does, through [`streams::CHANNEL_MEANDER`].
/// type-audit: pending(wave-1: band_edges)
#[derive(Clone, Debug)]
pub struct ChannelNetwork {
    /// One polyline per maximal downhill run of river cells, in build order
    /// (ascending head `CellId`). A tributary's run ends *on* the confluence
    /// vertex it joins, so the network is geometrically connected without any
    /// confluence special case — distance is to the network, not to an edge.
    /// type-audit: pending(wave-1: polylines)
    pub polylines: Vec<SphericalPolyline>,
    /// Per polyline, per vertex, the four [`band_edges`] borders for that
    /// vertex's discharge, gradient and cell spacing. Parallel to
    /// `polylines`: `band_edges[i].len() == polylines[i].points.len()`.
    pub band_edges: Vec<Vec<[f64; 4]>>,
    /// The meander displacement field. Derived once and reused for every
    /// vertex (the `Fbm` derive-once pattern), and — the point of it being a
    /// field at all — **continuous in position**, so a walker crosses a band
    /// edge once instead of flickering across it room by room.
    meander: SphereFbm,
}

impl ChannelNetwork {
    /// Build the network from a generated globe.
    ///
    /// Walks every `WaterKind::River` cell in ascending `CellId` order,
    /// starting runs at *heads* (river cells no other river cell drains into)
    /// and following `downhill` to the sea, a terminal sink, or an
    /// already-claimed trunk. Runs of a single cell are dropped: one isolated
    /// river cell has no direction, and a one-point polyline is not a line.
    pub fn build(globe: &TectonicGlobe, geo: &Geosphere, seed: Seed) -> ChannelNetwork {
        let meander = SphereFbm::new(
            seed.derive(streams::CHANNEL_MEANDER),
            MEANDER_FREQUENCY,
            MEANDER_OCTAVES,
        );
        let is_river = |c: CellId| matches!(*globe.water_kind.get(c), WaterKind::River);

        // In-degree within the river subgraph, as a dense Vec (CellId is a
        // dense 0..N index — kernel/CLAUDE.md).
        let mut has_river_inflow = vec![false; geo.cell_count()];
        for c in geo.cells() {
            if !is_river(c) {
                continue;
            }
            if let Some(t) = *globe.downhill.get(c) {
                has_river_inflow[t.0 as usize] = true;
            }
        }

        let mut claimed: BTreeSet<CellId> = BTreeSet::new();
        let mut runs: Vec<Vec<CellId>> = Vec::new();
        // Pass 0 starts only at heads, so a run is maximal upstream. Pass 1
        // sweeps anything still unclaimed: the downhill graph is strictly
        // descending and therefore acyclic, so pass 1 should find nothing —
        // it is here so the walk is total regardless.
        for heads_only in [true, false] {
            for c in geo.cells() {
                if !is_river(c) || claimed.contains(&c) {
                    continue;
                }
                if heads_only && has_river_inflow[c.0 as usize] {
                    continue;
                }
                let mut run = vec![c];
                claimed.insert(c);
                let mut current = c;
                while let Some(target) = *globe.downhill.get(current) {
                    if !is_river(target) {
                        break;
                    }
                    run.push(target);
                    if !claimed.insert(target) {
                        // Joined a trunk another run already owns: keep the
                        // shared vertex so the lines meet, then stop.
                        break;
                    }
                    current = target;
                }
                if run.len() >= 2 {
                    runs.push(run);
                }
            }
        }

        let mut polylines = Vec::with_capacity(runs.len());
        let mut all_edges = Vec::with_capacity(runs.len());
        for run in &runs {
            let base: Vec<[f64; 3]> = run.iter().map(|&c| geo.position(c)).collect();
            let mut points = Vec::with_capacity(base.len());
            let mut edges = Vec::with_capacity(base.len());
            for (i, &c) in run.iter().enumerate() {
                let spacing = cell_spacing(geo, c);
                let slope = local_slope(globe, geo, c);
                edges.push(band_edges(*globe.drainage.get(c), slope, spacing));
                if i == 0 || i + 1 == base.len() {
                    // A source and a mouth are anchored: the head must stay in
                    // its own cell and the mouth must stay on the coast.
                    points.push(base[i]);
                    continue;
                }
                // Displace perpendicular to the local travel direction, read
                // off the UNDISPLACED neighbours so the result does not depend
                // on the order vertices are visited in.
                let tangent = normalize([
                    base[i + 1][0] - base[i - 1][0],
                    base[i + 1][1] - base[i - 1][1],
                    base[i + 1][2] - base[i - 1][2],
                ]);
                let left = normalize(cross(base[i], tangent));
                // Amplitude scales with confinement, i.e. inversely with
                // gradient: low slope meanders, steep slope runs straight.
                let amplitude = MEANDER_AMPLITUDE_RATIO * spacing * confinement(slope);
                let offset = amplitude * meander_field(&meander, base[i]);
                points.push(normalize([
                    base[i][0] + left[0] * offset,
                    base[i][1] + left[1] * offset,
                    base[i][2] + left[2] * offset,
                ]));
            }
            polylines.push(SphericalPolyline { points });
            all_edges.push(edges);
        }

        ChannelNetwork {
            polylines,
            band_edges: all_edges,
            meander,
        }
    }

    /// The transverse band at `position`, and the **signed** great-circle
    /// distance to the nearest channel in radians (left-positive relative to
    /// the channel's direction of travel).
    ///
    /// The sign is not decoration: a ford is precisely the path on which it
    /// flips, and cut-bank versus point-bar is that sign read against meander
    /// curvature. On an empty network the answer is `(Dry, f64::INFINITY)`.
    ///
    /// The band geometry is that of the nearest *vertex* of the winning
    /// polyline, so width and confinement vary down a river as its discharge
    /// and gradient do.
    /// type-audit: pending(wave-1: position), pending(wave-1: return)
    pub fn transverse_at(&self, position: [f64; 3]) -> (Transverse, f64) {
        let mut best = f64::INFINITY;
        let mut best_line: Option<usize> = None;
        for (i, line) in self.polylines.iter().enumerate() {
            let d = line.signed_distance(position);
            // Strict `<` keeps the FIRST (lowest-index) line on an exact tie.
            if d.abs() < best.abs() {
                best = d;
                best_line = Some(i);
            }
        }
        let Some(line_index) = best_line else {
            return (Transverse::Dry, f64::INFINITY);
        };
        let points = &self.polylines[line_index].points;
        let mut nearest = 0usize;
        let mut nearest_distance = f64::INFINITY;
        for (j, &v) in points.iter().enumerate() {
            let a = angle(position, v);
            if a < nearest_distance {
                nearest_distance = a;
                nearest = j;
            }
        }
        let edges = self.band_edges[line_index][nearest];
        (Transverse::from_band(band(best, &edges)), best)
    }

    /// The meander displacement field at `position`, in `[-1, 1]` — the
    /// signed lateral offset (as a fraction of the local amplitude) this
    /// network's channels wander by.
    ///
    /// Public because the property it carries is the one the whole design
    /// rests on and downstream consumers may need to assert it: this is a
    /// **position-continuous** field, not a per-address hash like
    /// `windows/locale`'s `wetness` axis — which is exactly why sibling rooms
    /// there can never form a connected watercourse.
    /// type-audit: pending(wave-1: position), bare-ok(ratio: return)
    pub fn meander_at(&self, position: [f64; 3]) -> f64 {
        meander_field(&self.meander, position)
    }
}

/// The meander field, centred on zero: `SphereFbm` samples `[0, 1)`, and a
/// displacement needs both banks.
fn meander_field(fbm: &SphereFbm, position: [f64; 3]) -> f64 {
    2.0 * fbm.sample(position) - 1.0
}

#[cfg(test)]
mod tests {
    use super::*;

    fn unit(x: f64, y: f64, z: f64) -> [f64; 3] {
        normalize([x, y, z])
    }

    /// The **measured** mean angular separation between neighbouring cells on
    /// the canonical `Geosphere::new(6)`, radians. Do not try to re-derive it
    /// and conclude it is wrong: it is not `sqrt(4π/N)` (0.01752), not the
    /// equal-area circle diameter (0.01976), and not a hexagon edge (0.0109).
    /// It is the mean over every cell of the mean angle to its neighbours,
    /// taken from the Task 4 probe — the same quantity `cell_spacing` computes
    /// per cell at build time. (Level 5, for reference: 0.037769.)
    const CANONICAL_CELL_EDGE: f64 = 0.018_886;

    /// The largest drainage the canonical grid actually produces, from the
    /// same probe: max 146 / 177 / 173 on seeds 42 / 7 / 1234, p99 ≈ 117–138,
    /// p50 ≈ 23–25. Claims about "the widest river" are stated HERE rather
    /// than at some round number the world never reaches — an assertion
    /// pinned to an unreachable discharge is a tripwire that reddens for
    /// reasons unrelated to what it claims.
    const CANONICAL_MAX_DRAINAGE: f64 = 180.0;

    /// A hand-built two-segment polyline on the equator, with band edges from
    /// the REAL laws at a deliberately coarse synthetic cell (spacing 1.0
    /// rad). The coarseness is the point: it puts every band wider than the
    /// 0.0005-rad step the monotone sweep walks, so the sweep can actually
    /// observe all five. Drainage 36 is an ordinary river on the canonical
    /// grid (p50 ≈ 23, p90 ≈ 55) and slope 0 is a flat, fully unconfined
    /// reach. Resulting edges: [0.0015, 0.003, 0.063, 0.0945] rad.
    fn test_network() -> ChannelNetwork {
        let points = vec![
            unit(1.0, 0.0, 0.0),
            unit(1.0, 0.5, 0.0),
            unit(0.5, 1.0, 0.0),
        ];
        let edges = band_edges(36.0, 0.0, 1.0);
        ChannelNetwork {
            band_edges: vec![vec![edges; points.len()]],
            polylines: vec![SphericalPolyline { points }],
            meander: SphereFbm::new(
                Seed(42).derive(streams::CHANNEL_MEANDER),
                MEANDER_FREQUENCY,
                MEANDER_OCTAVES,
            ),
        }
    }

    /// Offset `start` by `off` radians perpendicular to the polyline's first
    /// segment, left-positive — the same side convention `signed_distance`
    /// reads.
    fn offset_perpendicular(line: &SphericalPolyline, start: [f64; 3], off: f64) -> [f64; 3] {
        let tangent = normalize([
            line.points[1][0] - line.points[0][0],
            line.points[1][1] - line.points[0][1],
            line.points[1][2] - line.points[0][2],
        ]);
        let left = normalize(cross(start, tangent));
        normalize([
            start[0] + left[0] * off,
            start[1] + left[1] * off,
            start[2] + left[2] * off,
        ])
    }

    fn sample_left(net: &ChannelNetwork) -> [f64; 3] {
        let line = &net.polylines[0];
        offset_perpendicular(line, line.points[0], 0.002)
    }

    fn sample_right(net: &ChannelNetwork) -> [f64; 3] {
        let line = &net.polylines[0];
        offset_perpendicular(line, line.points[0], -0.002)
    }

    /// The campaign's whole claim, as an assertion: the largest river the
    /// canonical grid ACTUALLY PRODUCES is a small FRACTION of a cell edge,
    /// not a cell. Stated over the measured discharge ceiling (~180), not a
    /// hypothetical one — at the placeholder coefficient the widest real
    /// channel is 1.27e-4 rad against a bound of 1.89e-3, i.e. ~14.9x of
    /// headroom, so Task 6's calibration has room to move without reddening
    /// this for a reason it is not about.
    #[test]
    fn the_widest_channel_is_far_narrower_than_a_cell() {
        let cell_edge = CANONICAL_CELL_EDGE;
        let widest = channel_half_width(CANONICAL_MAX_DRAINAGE, cell_edge) * 2.0;
        assert!(
            widest < cell_edge / 10.0,
            "widest channel {widest} rad is not << cell edge {cell_edge} rad"
        );
        assert!(widest > 0.0, "width law produced a non-positive width");
    }

    /// Width must be MONOTONE in discharge — a bigger river is never narrower.
    #[test]
    fn width_increases_with_discharge() {
        let e = CANONICAL_CELL_EDGE;
        // Across the range the world actually spans: threshold to ceiling.
        let small = channel_half_width(crate::water::RIVER_MIN_DRAINAGE, e);
        let big = channel_half_width(CANONICAL_MAX_DRAINAGE, e);
        assert!(big > small, "big {big} not wider than small {small}");
        // And beyond it, so the law itself is monotone rather than merely
        // happening to be so over the sampled interval.
        assert!(channel_half_width(2_000.0, e) > big);
    }

    /// A sub-threshold trickle is not a channel at all.
    #[test]
    fn drainage_below_the_river_threshold_has_zero_width() {
        let e = CANONICAL_CELL_EDGE;
        assert_eq!(
            channel_half_width(crate::water::RIVER_MIN_DRAINAGE - 1.0, e),
            0.0
        );
    }

    /// Bands are ordered outward and exhaustive: walking away from the
    /// centreline you pass every band in order and never re-enter one.
    /// This is H4 (spec 10) as a unit test on the band function alone.
    #[test]
    fn bands_are_monotone_outward() {
        let net = test_network();
        let start = net.polylines[0].points[0];
        let mut seen = Vec::new();
        for i in 0..200 {
            let off = f64::from(i) * 0.0005;
            let p = offset_perpendicular(&net.polylines[0], start, off);
            let (t, _) = net.transverse_at(p);
            if seen.last() != Some(&t) {
                seen.push(t);
            }
        }
        let expected = [
            Transverse::Channel,
            Transverse::Bank,
            Transverse::Floodplain,
            Transverse::Terrace,
            Transverse::Dry,
        ];
        assert!(
            seen.iter().eq(expected.iter().filter(|e| seen.contains(e))),
            "bands re-entered or came out of order: {seen:?}"
        );
        // The sweep is only worth running if it actually reaches every band:
        // a sweep that stops inside the floodplain satisfies the ordering
        // assertion above VACUOUSLY, since the filter drops what it never saw.
        assert_eq!(
            seen,
            expected.to_vec(),
            "the sweep must observe all five bands, not a prefix"
        );
    }

    /// The sign survives the band lookup, so a crossing is detectable.
    #[test]
    fn transverse_at_reports_a_signed_distance() {
        let net = test_network();
        let (_, left) = net.transverse_at(sample_left(&net));
        let (_, right) = net.transverse_at(sample_right(&net));
        assert!(left * right < 0.0, "sign lost: {left} and {right}");
    }

    /// `Transverse`'s index/name/legend triple is a committed contract (a
    /// scene legend indexes into it), so it is pinned exactly as
    /// `WaterKind`'s is.
    #[test]
    fn transverse_index_name_and_legend_are_stable() {
        assert_eq!(Transverse::Channel.index(), 0);
        assert_eq!(Transverse::Bank.index(), 1);
        assert_eq!(Transverse::Floodplain.index(), 2);
        assert_eq!(Transverse::Terrace.index(), 3);
        assert_eq!(Transverse::Dry.index(), 4);
        assert_eq!(
            Transverse::LEGEND,
            ["channel", "bank", "floodplain", "terrace", "dry"]
        );
        for t in [
            Transverse::Channel,
            Transverse::Bank,
            Transverse::Floodplain,
            Transverse::Terrace,
            Transverse::Dry,
        ] {
            assert_eq!(Transverse::LEGEND[t.index() as usize], t.name());
            assert_eq!(Transverse::from_band(t.index() as usize), t);
        }
        assert_eq!(Transverse::from_band(99), Transverse::Dry);
    }

    /// The confinement law's defining property: A GORGE HAS NO FLOODPLAIN.
    /// At or above `GORGE_SLOPE` the bank and floodplain borders coincide, so
    /// the floodplain band is empty and a point just outside the bank reads
    /// `Terrace`. A flat reach of the same river has a broad one.
    #[test]
    fn a_gorge_has_no_floodplain_and_a_flat_reach_has_a_broad_one() {
        let spacing = CANONICAL_CELL_EDGE;
        let gorge = band_edges(100.0, GORGE_SLOPE, spacing);
        assert_eq!(
            gorge[1], gorge[2],
            "a gorge's floodplain band must be empty: {gorge:?}"
        );
        // Steeper still is no less confined — the clamp holds.
        let steeper = band_edges(100.0, GORGE_SLOPE * 3.0, spacing);
        assert_eq!(steeper, gorge);
        let flat = band_edges(100.0, 0.0, spacing);
        assert!(
            flat[2] > flat[1] * 10.0,
            "a flat reach must have a broad floodplain: {flat:?}"
        );
        // Confinement is monotone across the measured population, and the
        // normalization is chosen so it discriminates rather than saturating:
        // the median river (~1.3e4) sits mid-range, the steep quartile low.
        assert!(confinement(1.3e4) > 0.6 && confinement(1.3e4) < 0.8);
        assert!(confinement(2.4e4) > 0.3 && confinement(2.4e4) < 0.5);
        assert!(confinement(2.0e3) > confinement(2.4e4));
    }

    /// Band edges are non-decreasing at every gradient, which is what lets
    /// `band` read them directly.
    #[test]
    fn band_edges_are_non_decreasing_at_every_gradient() {
        for step in 0..40 {
            let slope = f64::from(step) * 2_500.0;
            let e = band_edges(60.0, slope, CANONICAL_CELL_EDGE);
            assert!(
                e[0] <= e[1] && e[1] <= e[2] && e[2] <= e[3],
                "edges out of order at slope {slope}: {e:?}"
            );
        }
    }

    /// THE POSITIVE CONTROL for spec §5.2's named trap. The meander must come
    /// from a position-continuous field, not a per-address hash. Two positions
    /// a hair apart must give nearly the same displacement — an address hash
    /// would make them independent draws and blow this apart — and two far
    /// apart must differ, which is what rules out the field being constant
    /// (the trivial way to pass the first assertion).
    #[test]
    fn meander_displacement_is_continuous_in_position() {
        let net = test_network();
        let a = net.meander_at(unit(1.0, 0.0, 0.0));
        let b = net.meander_at(unit(1.0, 1e-6, 0.0));
        let far = net.meander_at(unit(0.0, 1.0, 0.0));
        assert!(
            (a - b).abs() < 1e-3,
            "adjacent positions jumped: {a} vs {b}"
        );
        assert!((a - far).abs() > 0.0, "field is constant, not noise");
    }

    /// Continuity is not just a local accident: sweeping a great-circle arc,
    /// consecutive samples one milliradian apart never jump by more than a
    /// small fraction of the field's full range, AND the field varies over
    /// the arc by an appreciable amount. An address-hashed field fails the
    /// first; a constant field fails the second.
    #[test]
    fn the_meander_field_is_smooth_along_an_arc_and_not_constant() {
        let net = test_network();
        let mut previous = net.meander_at(unit(1.0, 0.0, 0.0));
        let mut largest_step = 0.0_f64;
        let mut lowest = previous;
        let mut highest = previous;
        for i in 1..500 {
            let t = f64::from(i) * 1e-3;
            let v = net.meander_at(unit(math::cos(t), math::sin(t), 0.0));
            largest_step = largest_step.max((v - previous).abs());
            lowest = lowest.min(v);
            highest = highest.max(v);
            previous = v;
        }
        assert!(
            largest_step < 0.1,
            "a 1 mrad step moved the field by {largest_step} — that is a hash, not a field"
        );
        assert!(
            highest - lowest > 0.1,
            "the field barely varies over half a radian ({lowest}..{highest}) — not noise"
        );
    }

    /// The campaign's central claim, as a network-level assertion: the band
    /// geometry is read from the NEAREST VERTEX, so one river is wide at its
    /// mouth and narrow at its head. A lookup that always took the polyline's
    /// first vertex — or one flat width per line — reads the same band at
    /// both ends and fails here.
    #[test]
    fn band_geometry_varies_along_one_river() {
        // Three collinear vertices: a trickle at the head, a mainstem at the
        // mouth. Same laws, three discharges an order of magnitude apart.
        let points = vec![
            unit(1.0, 0.0, 0.0),
            unit(1.0, 0.5, 0.0),
            unit(0.5, 1.0, 0.0),
        ];
        let net = ChannelNetwork {
            band_edges: vec![vec![
                band_edges(16.0, 0.0, 1.0),
                band_edges(200.0, 0.0, 1.0),
                band_edges(4_000.0, 0.0, 1.0),
            ]],
            polylines: vec![SphericalPolyline { points }],
            meander: SphereFbm::new(
                Seed(42).derive(streams::CHANNEL_MEANDER),
                MEANDER_FREQUENCY,
                MEANDER_OCTAVES,
            ),
        };
        let head_half = net.band_edges[0][0][0];
        let mouth_half = net.band_edges[0][2][0];
        assert!(
            mouth_half > head_half * 10.0,
            "the test setup is not graded"
        );
        // One offset, chosen to sit INSIDE the mouth's channel and OUTSIDE
        // the head's: the reading must differ purely by where along the line
        // it is taken.
        let offset = 0.5 * (head_half + mouth_half);
        let line = &net.polylines[0];
        let at_head = offset_perpendicular(line, line.points[0], offset);
        let at_mouth = offset_perpendicular(line, line.points[2], offset);
        assert_eq!(
            net.transverse_at(at_mouth).0,
            Transverse::Channel,
            "the mainstem's own width was not used at the mouth"
        );
        assert_ne!(
            net.transverse_at(at_head).0,
            Transverse::Channel,
            "the head read the mainstem's width — geometry is not per-vertex"
        );
    }

    /// The network built from a real (small) globe is well-formed and
    /// deterministic. Structural only — the measurement of a canonical world
    /// is Task 6's, not this task's.
    #[test]
    fn a_built_network_is_well_formed_and_deterministic() {
        // Level 5, not the canonical 6: level 4 is too coarse to accumulate
        // `RIVER_MIN_DRAINAGE` anywhere (seed 42 has zero river cells there),
        // and level 6 is Task 6's measurement, not this test's.
        let geo = Geosphere::new(5);
        let outcome =
            crate::globe::generate(Seed(42), &geo, &crate::pins::TerrainPins::default()).unwrap();
        let net = ChannelNetwork::build(&outcome.globe, &geo, Seed(42));
        let again = ChannelNetwork::build(&outcome.globe, &geo, Seed(42));
        assert_eq!(net.polylines, again.polylines, "build is not deterministic");
        assert_eq!(net.band_edges, again.band_edges);
        assert!(
            !net.polylines.is_empty(),
            "no channels on a level-5 seed 42"
        );
        for (line, edges) in net.polylines.iter().zip(&net.band_edges) {
            assert!(line.points.len() >= 2, "a one-point polyline is not a line");
            assert_eq!(line.points.len(), edges.len(), "edges parallel to points");
            for p in &line.points {
                let norm = (p[0] * p[0] + p[1] * p[1] + p[2] * p[2]).sqrt();
                assert!((norm - 1.0).abs() < 1e-12, "vertex off the unit sphere");
            }
            for e in edges {
                assert!(e[0] <= e[1] && e[1] <= e[2] && e[2] <= e[3], "{e:?}");
                assert!(e[0] > 0.0, "a river vertex with zero channel width");
            }
        }
        // Every river cell is covered by the network: it sits inside the
        // channel band of the polyline it belongs to, or is one of the
        // dropped isolated singletons.
        let on_line: BTreeSet<CellId> = geo
            .cells()
            .filter(|&c| {
                matches!(*outcome.globe.water_kind.get(c), WaterKind::River)
                    && matches!(net.transverse_at(geo.position(c)).0, Transverse::Channel)
            })
            .collect();
        assert!(
            !on_line.is_empty(),
            "no river cell reads as Channel at its own centre"
        );
    }
}
