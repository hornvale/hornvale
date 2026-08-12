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

/// The `a` of `w = a·Q^b`, as a fraction of the local cell spacing.
///
/// **Calibrated once, in the open** (spec §10, H1): fitted so the widest
/// channel seed 42 actually produces on the canonical `Geosphere::new(6)` is
/// **1/100 of a canonical cell edge** across. That world's largest drainage
/// is 146, and at the pre-fit placeholder `5.0e-4` its widest channel was
/// 1.1127e-4 rad — 1/169.7 of the 0.018886-rad canonical edge. Scaling to hit
/// 1/100 exactly wants 8.4864e-4; `8.5e-4` is that rounded to two figures and
/// lands the widest channel at 1/99.8 of an edge, which is inside the width
/// law's own precision.
///
/// The cell edge very nearly cancels out of the fit (`w = a·edge·√Q` against
/// a target of `edge/100` gives `a ≈ 1/(100·√Q_max)` = 8.276e-4); the 2.5%
/// residual is the local spacing at the widest vertex differing from the
/// world-mean edge. So this coefficient is essentially a statement about the
/// largest discharge the terrain produces, not about the grid.
///
/// **The exponent, not this, is the dynamic-range lever.** Real drainage
/// spans 15 to ~180, so `√Q` spans only ~3.5× across the whole
/// headwater-to-mainstem range; widening the range by moving `a` is not
/// possible — it scales every channel equally.
/// type-audit: bare-ok(ratio)
pub const CHANNEL_WIDTH_COEFF: f64 = 8.5e-4;

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
    /// (ascending head `CellId`). A tributary's run ends *on* the cell where
    /// it joins its trunk, and `build`'s explicit **confluence repair** pass
    /// then places that mouth vertex exactly on the trunk's own vertex for
    /// that cell, so the network is geometrically connected.
    ///
    /// This doc previously said the shared cell alone made the network
    /// connected "without any confluence special case". That was the false
    /// inference — shared *cell* does not imply shared *point* — and it is
    /// what produced the H2 defect: the tributary's mouth was anchored at the
    /// cell's undisplaced position while the trunk's vertex for the same cell
    /// was meander-displaced, leaving the two runs a median 4.5 channel
    /// half-widths apart. The special case exists precisely because geometric
    /// connectedness does not follow from graph connectedness, and a reader
    /// who assumes it does will re-introduce the same defect.
    /// type-audit: pending(wave-1: polylines)
    pub polylines: Vec<SphericalPolyline>,
    /// Per polyline, per vertex, the four [`band_edges`] borders for that
    /// vertex's discharge, gradient and cell spacing. Parallel to
    /// `polylines`: `band_edges[i].len() == polylines[i].points.len()`.
    pub band_edges: Vec<Vec<[f64; 4]>>,
    /// Per polyline, per vertex, the cell that vertex was placed from — the
    /// downhill run the polyline is a rendering of. Parallel to `polylines`:
    /// `run_cells[i].len() == polylines[i].points.len()`.
    ///
    /// Published because a polyline's *geometry* alone cannot say where two
    /// lines meet. Since the confluence repair a tributary's mouth vertex is
    /// placed **exactly** on the trunk's vertex for the shared cell, so a
    /// consumer could in principle recover the join by looking for coincident
    /// points — but that is an inference from a float equality, and it cannot
    /// tell a confluence from two lines that merely pass through the same
    /// place. The cell correspondence states the topology outright, which is
    /// what a longitudinal-connectivity measurement needs if it is not to be
    /// measuring its own guess.
    pub run_cells: Vec<Vec<CellId>>,
    /// The meander displacement field. Derived once and reused for every
    /// vertex (the `Fbm` derive-once pattern), and — the point of it being a
    /// field at all — **continuous in position**, so a walker crosses a band
    /// edge once instead of flickering across it room by room.
    meander: SphereFbm,
}

/// One channel reading at one position: what
/// [`ChannelNetwork::bank_reading`] selected, all of it, from a single
/// vertex selection.
///
/// The four fields travel together because they are *one* answer to "which
/// reach applies here". Splitting them across separate queries is the defect
/// this type exists to prevent: each query would re-run the winning-line
/// tie-break and the nearest-vertex scan, and would then agree with the
/// others only by luck.
///
/// **`cell` and `line` are in-process handles, not document fields.** `cell`
/// names the canonical grid cell the winning vertex was placed from, so a
/// caller can read that reach's discharge (`GeneratedTerrain::drainage_at`)
/// without searching for the vertex a second time; `line` names the polyline
/// the reading is *about*, so a caller holding two readings can tell whether
/// they concern the same channel. Both are build-order-adjacent identity and
/// **neither may ever be serialized**; the durable things here are the signed
/// distance and the edges.
///
/// An earlier draft carried `cell` and withheld `line`, on the grounds that
/// the polyline index is build order. So is `cell`, and the argument for
/// publishing one is the argument for publishing the other — the rule the
/// campaign actually holds is *never serialize either*, which omission does
/// not enforce. What omission did instead was push a consumer comparing two
/// readings into re-running [`ChannelNetwork::nearest_line`] itself, which is
/// the duplicate-selection defect this type exists to prevent.
/// type-audit: pending(wave-1: signed_distance), pending(wave-1: band_edges), bare-ok(index: line)
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct BankReading {
    /// Angular distance to the nearest channel in radians, **positive on the
    /// left bank facing downstream** — the number
    /// [`ChannelNetwork::bank_signed_distance`] reports, and meaningful in
    /// its sign only inside `band_edges` (see that method's own doc).
    pub signed_distance: f64,
    /// The four [`band_edges`] borders of the nearest vertex of the winning
    /// polyline: channel/bank, bank/floodplain, floodplain/terrace,
    /// terrace/dry.
    pub band_edges: [f64; 4],
    /// The canonical grid cell that vertex was placed from — the reach whose
    /// discharge and gradient produced `band_edges`.
    pub cell: CellId,
    /// The winning polyline's index in [`ChannelNetwork::polylines`] — the
    /// same index [`ChannelNetwork::nearest_line`] reports. An in-process
    /// handle, **never serialized**: two readings are about the same channel
    /// only if this agrees, and nothing else in the reading can say so.
    pub line: usize,
}

impl ChannelNetwork {
    /// Build the network from a generated globe.
    ///
    /// Walks every `WaterKind::River` cell in ascending `CellId` order,
    /// starting runs at *heads* (river cells no other river cell drains into)
    /// and following `downhill` to the sea, a terminal sink, or an
    /// already-claimed trunk. Runs of a single cell are dropped: one isolated
    /// river cell has no direction, and a one-point polyline is not a line.
    ///
    /// A final pass moves every **confluence mouth** onto the trunk vertex it
    /// joins, so runs that meet in the drainage graph also meet in space; see
    /// the comment on that pass for the measurement that made it necessary.
    ///
    /// `meander_seed` must already be the derived `streams::CHANNEL_MEANDER`
    /// leg (a caller holding the terrain-root seed derives it itself, the
    /// same way every other hash-noise-only leg in this crate is derived
    /// once and stored — see `TectonicGlobe::channel_noise_seed`). `build`
    /// does not derive it again: a caller handed the ROOT seed here would
    /// silently grant this network's build path the ability to derive any
    /// other terrain stream, which is exactly the leak `channel_seed`'s own
    /// doc warns against.
    pub fn build(globe: &TectonicGlobe, geo: &Geosphere, meander_seed: Seed) -> ChannelNetwork {
        let meander = SphereFbm::new(meander_seed, MEANDER_FREQUENCY, MEANDER_OCTAVES);
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
                    // its own cell and the mouth must stay on the coast. A
                    // mouth that is a CONFLUENCE rather than a coast is moved
                    // onto its trunk by the pass below.
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

        // THE CONFLUENCE REPAIR. A tributary's mouth sits on a cell the TRUNK
        // carries as an interior vertex, so the anchoring rule above would
        // leave the tributary's copy at the cell's undisplaced position while
        // the trunk's copy is meander-displaced. The two runs are then joined
        // in the drainage graph and separated in space — measured at a median
        // 4.5 channel half-widths on seed 42, which put a walker out of the
        // water at 12 of 15 joins and is what falsified the campaign's
        // longitudinal-connectivity prediction (spec 10, H2). Placing the
        // mouth on the trunk's own vertex makes the two lines meet exactly.
        //
        // `owner[c]` is the (line, vertex) of the run that CLAIMED `c` and
        // continued past it, never one that merely terminates on it. That
        // distinction is the whole of confluence topology, and it makes the
        // map unambiguous: once a cell is claimed only the claiming run walks
        // on, so a cell is a non-final vertex of at most one run. A mouth
        // whose cell no run continues from is a REAL mouth — the sea or a
        // terminal sink — and stays anchored where it is.
        //
        // Order-independent by construction: only final vertices are moved and
        // only non-final vertices are read, so no relocation can be the source
        // of another.
        let mut owner: Vec<Option<(usize, usize)>> = vec![None; geo.cell_count()];
        for (i, run) in runs.iter().enumerate() {
            for (j, &c) in run.iter().enumerate() {
                if j + 1 < run.len() {
                    owner[c.0 as usize] = Some((i, j));
                }
            }
        }
        for i in 0..polylines.len() {
            let last = polylines[i].points.len() - 1;
            let Some((trunk, vertex)) = owner[runs[i][last].0 as usize] else {
                continue;
            };
            if trunk != i {
                polylines[i].points[last] = polylines[trunk].points[vertex];
            }
        }

        ChannelNetwork {
            polylines,
            band_edges: all_edges,
            run_cells: runs,
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
        let Some(reading) = self.bank_reading(position) else {
            return (Transverse::Dry, f64::INFINITY);
        };
        (
            Transverse::from_band(band(reading.signed_distance, &reading.band_edges)),
            reading.signed_distance,
        )
    }

    /// The whole reading at `position`: the **signed** distance
    /// [`ChannelNetwork::bank_signed_distance`] reports, paired with the four
    /// [`band_edges`] borders that apply *there* — those of the nearest vertex
    /// of the winning polyline — and the cell that vertex was placed from.
    /// `None` on an empty network.
    ///
    /// **This is the one implementation of "which vertex's edges apply here",
    /// and it exists so that there can only be one.**
    /// [`ChannelNetwork::transverse_at`] classifies through it and a consumer
    /// that stores the pair reads it from here, so the edges a document
    /// carries are by construction the edges the classification was made
    /// with. The alternative — a caller re-selecting the vertex itself — would
    /// have to duplicate both the winning-line tie-break (which lives in
    /// [`ChannelNetwork::nearest_line`], and reaches the *sign*) and the
    /// nearest-vertex scan below, and would then agree with `transverse_at`
    /// only by luck. That is the same argument
    /// [`ChannelNetwork::bank_signed_distance`] makes for delegating rather
    /// than re-deriving.
    ///
    /// The vertex scan is by *undisplaced angular separation* from the vertex,
    /// not by the segment the signed distance was measured against: band
    /// geometry is a per-vertex property (discharge, gradient, cell spacing),
    /// and the nearest vertex is the reach whose hydraulics a point actually
    /// sits in.
    ///
    /// [`BankReading::cell`] is here for the same reason the edges are: a
    /// consumer asking whether a channel can be forded needs that reach's
    /// **discharge** as well as its width, and a second nearest-vertex search
    /// to find it would be a second chance to answer about a different reach
    /// than the bands describe. [`BankReading::line`] is here for the same
    /// reason once more: a consumer comparing *two* readings — the shape every
    /// crossing query has — must be able to ask whether they are about the
    /// same channel, and a second `nearest_line` call to find out would be the
    /// duplicate selection this method exists to remove.
    /// type-audit: pending(wave-1: position)
    pub fn bank_reading(&self, position: [f64; 3]) -> Option<BankReading> {
        let (line_index, signed) = self.nearest_line(position)?;
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
        Some(BankReading {
            signed_distance: signed,
            band_edges: self.band_edges[line_index][nearest],
            cell: self.run_cells[line_index][nearest],
            line: line_index,
        })
    }

    /// The polyline [`ChannelNetwork::transverse_at`] would answer from at
    /// `position`, and the **signed** distance to it — `None` on an empty
    /// network.
    ///
    /// Published because *which* channel a reading came from is not
    /// recoverable from `(Transverse, f64)` alone, and a consumer that
    /// re-derives it re-derives the tie-break too. A transverse profile that
    /// re-enters `Channel` is a defect if it is the SAME channel and an
    /// ordinary neighbouring river if it is not; only this can tell them
    /// apart. Strict `<` keeps the first (lowest-index) line on an exact tie,
    /// which is the whole of the tie-break contract.
    ///
    /// That tie-break is the one place where build order still reaches the
    /// **sign**: on an exact `|d|` tie between two lines the lowest index
    /// wins, and the winner's downstream direction is what
    /// [`ChannelNetwork::bank_signed_distance`] then reports. The set of
    /// positions equidistant from two lines has measure zero and this is
    /// correct as it stands — but the campaign's rule is that the line index
    /// is never serialized while the sign it selects will be, so it is worth
    /// knowing the two are not entirely independent.
    /// type-audit: pending(wave-1: position), pending(wave-1: return)
    pub fn nearest_line(&self, position: [f64; 3]) -> Option<(usize, f64)> {
        let mut best = f64::INFINITY;
        let mut best_line: Option<usize> = None;
        for (i, line) in self.polylines.iter().enumerate() {
            let d = line.signed_distance(position);
            if d.abs() < best.abs() {
                best = d;
                best_line = Some(i);
            }
        }
        best_line.map(|i| (i, best))
    }

    /// Angular distance from `position` to the nearest channel in radians,
    /// **positive on the left bank facing downstream** and negative on the
    /// right. `None` on an empty network — there is no bank to be on.
    ///
    /// This is the name stage 2 stores a channel distance under, and the name
    /// exists for the referent rather than the arithmetic. The number is the
    /// one [`ChannelNetwork::nearest_line`] already reports; what this method
    /// adds is a meaning for its **sign** that survives a rebuild.
    ///
    /// **Why "left of travel" is "left of downstream".** [`SphericalPolyline`]
    /// documents its sign as left-positive relative to the winning segment's
    /// direction of travel, and travel direction is vertex order. That alone
    /// is not a durable referent: the polyline *index* is build order and must
    /// never be serialized. But the vertex order within a line is not an
    /// accident — [`ChannelNetwork::build`] starts every run at a head and
    /// appends each `downhill` target in turn, so `run_cells[i]` and the
    /// parallel `polylines[i].points` run downstream, from source toward the
    /// sea. Left of travel is therefore left facing downstream: hydrology's
    /// own convention, the term a person would use, and stable across builds
    /// because it is a property of the flow graph rather than of the walk that
    /// rendered it.
    ///
    /// That is a property of `build`'s loop, not of any type here, so
    /// `channel_properties.rs::the_polyline_vertex_order_is_downstream_order`
    /// asserts it on a real world. A change that collected a run upstream
    /// would swap every bank in the world and break nothing else.
    ///
    /// # WHERE THE SIGN MEANS ANYTHING
    ///
    /// **Only within the banded neighbourhood of the winning segment.** This
    /// is a signed distance to the nearest of many *open arcs*, and such a
    /// field changes sign on surfaces that have nothing to do with water:
    /// beyond a line's endpoint, and along the bisector between two arcs that
    /// meet. A sign change out there is a fact about the polyline soup, not
    /// about a river. **Read the sign only where `|d|` is small — inside the
    /// [`band_edges`] of the reading, or equivalently where
    /// [`ChannelNetwork::transverse_at`] does not answer `Dry`.** A consumer
    /// that treats *any* sign change as a crossing (a ford, say) will report
    /// one on dry ground at every river source and every river mouth.
    ///
    /// Measured on seed 42 at `Geosphere::new(5)` — 13 lines, 46 vertices, 26
    /// endpoints, 1 confluence, terrace edge (the outermost band) 2.0e-4 rad
    /// at its narrowest, 3.5e-3 median, **6.9e-3 at its widest**:
    ///
    /// - A 720-point circle of radius **1.0e-2 rad** about each endpoint gives
    ///   54 sign changes. 29 are real crossings, inside the bands. **25 sit at
    ///   `|d|` = 1.0e-2 — the circle's own radius** — one for each of the 25
    ///   true endpoints (the 26th is a confluence mouth). That is farther from
    ///   water than any band edge in the world.
    /// - The locus is a *ray*, not a place: probing at radius 5.0e-3 finds the
    ///   same 54 flips with `|d|` ≤ 5.0e-3. It extends outward without bound,
    ///   so **no fixed distance threshold makes it go away** — only asking
    ///   whether the reading is inside its own bands does.
    /// - The confluence gives 4 flips at radius 1.0e-2: three real ones at
    ///   `|d|` ≈ 3e-5, and **one on the bisector between two branches at
    ///   `|d|` = 4.8e-3** (2.4e-3 at radius 5.0e-3 — it too scales with the
    ///   probe). Note that 4.8e-3 is *inside* the widest terrace edge, so a
    ///   band test only excludes it if the band is a narrow one. Gate a
    ///   crossing on `Channel`/`Bank`, not on the terrace.
    ///
    /// None of this is introduced here — it is inherent to the quantity, and
    /// [`ChannelNetwork::transverse_at`] has always had it. It is written down
    /// here because this is the method whose *name* promises the sign means
    /// something, and stage 2 stores that sign in a document.
    ///
    /// # Implementation note
    ///
    /// Delegating to `nearest_line` rather than re-deriving the sign from the
    /// winning segment is deliberate: the tie-break between equidistant
    /// segments and the side a **degenerate** segment borrows from its
    /// neighbour both live in the kernel, and meander displacement can
    /// collapse adjacent vertices, so that second path is reachable. A second
    /// implementation here would be a second chance to disagree with it.
    /// type-audit: pending(wave-1: position), pending(wave-1: return)
    pub fn bank_signed_distance(&self, position: [f64; 3]) -> Option<f64> {
        self.nearest_line(position).map(|(_, signed)| signed)
    }

    /// The widest channel half-width anywhere in the network, radians
    /// (`0.0` on an empty network). The max, over every vertex of every
    /// polyline, of [`band_edges`]'s first border — the whole-network
    /// analogue of [`channel_half_width`] for a caller (Task 5's provider
    /// property test) that wants one number rather than a per-vertex query.
    /// type-audit: pending(wave-1: return)
    pub fn widest_half_width(&self) -> f64 {
        self.band_edges
            .iter()
            .flatten()
            .map(|edges| edges[0])
            .fold(0.0_f64, f64::max)
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
    use crate::streams;

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

    /// The **measured** number of confluences on seeds 42 and 7 at the
    /// canonical level 6 (15 + 52) — the population the confluence-repair
    /// test asserts over. A datum about the terrain, not a threshold anyone
    /// chose.
    ///
    /// Level 6 rather than the level 5 the rest of this file uses, because
    /// level 5 does not have the phenomenon: seeds 42, 7 and 1234 together
    /// produce exactly **one** confluence there, so a test on that grid would
    /// assert over a single join however many worlds it swept. The test does
    /// not call `transverse_at`, so it pays only for genesis — measured at
    /// 0.63 s for both worlds, in line with the rest of this suite.
    const CONFLUENCES_AT_LEVEL_6: usize = 67;

    /// A hand-built two-segment polyline on the equator, with band edges from
    /// the REAL laws at a deliberately coarse synthetic cell (spacing 1.0
    /// rad). The coarseness is the point: it puts every band wide enough for
    /// the monotone sweep to resolve. Drainage 36 is an ordinary river on the
    /// canonical grid (p50 ≈ 23, p90 ≈ 55) and slope 0 is a flat, fully
    /// unconfined reach. Resulting edges at the calibrated coefficient:
    /// [0.00255, 0.00765, 0.10965, 0.164475] rad — 1.7x the pre-calibration
    /// figures, which is why the sweep below takes its reach from
    /// [`band_edges`] rather than from a fixed step count.
    fn test_network() -> ChannelNetwork {
        let points = vec![
            unit(1.0, 0.0, 0.0),
            unit(1.0, 0.5, 0.0),
            unit(0.5, 1.0, 0.0),
        ];
        let edges = band_edges(36.0, 0.0, 1.0);
        ChannelNetwork {
            band_edges: vec![vec![edges; points.len()]],
            run_cells: vec![(0..points.len() as u32).map(CellId).collect()],
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
    /// hypothetical one. Task 6's calibration moved the coefficient 1.70x
    /// (5.0e-4 -> 8.5e-4) and this test stayed green with room to spare: the
    /// widest channel at the ceiling discharge is now 2.154e-4 rad against a
    /// bound of 1.889e-3, i.e. ~8.8x of headroom, down from ~14.9x. The
    /// campaign's claim is a bound, not the calibration target — the target
    /// (1/100 of an edge, at seed 42's REAL max discharge of 146) sits an
    /// order of magnitude inside it, which is why fitting one did not
    /// threaten the other.
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
    ///
    /// The sweep's REACH is derived from the outermost band edge rather than
    /// hardcoded. It used to be 200 fixed 0.0005-rad steps, and Task 6's
    /// calibration (`CHANNEL_WIDTH_COEFF` 5.0e-4 -> 8.5e-4) scaled the bands
    /// past the end of it — the sweep stopped inside the floodplain and the
    /// anti-vacuity assertion below caught it. Deriving the reach fixes that
    /// for the right reason: the claim being tested is about ORDER, not about
    /// any particular width, so the sweep should follow the widths wherever
    /// calibration puts them instead of having to be re-tuned alongside.
    #[test]
    fn bands_are_monotone_outward() {
        let net = test_network();
        let start = net.polylines[0].points[0];
        let steps = 400;
        // Past the terrace, so `Dry` is reachable at any calibration.
        let reach = net.band_edges[0][0][3] * 1.2;
        let mut seen = Vec::new();
        for i in 0..=steps {
            let off = reach * f64::from(i) / f64::from(steps);
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

    /// An empty network has no bank to be on, and must say so rather than
    /// answer a number. `transverse_at` can report `f64::INFINITY` because it
    /// pairs it with `Dry`; a bare distance has no such companion, and `0.0`
    /// would read to a consumer banding it as "in the water".
    ///
    /// Constructed here rather than in `channel_properties.rs` because the
    /// `meander` field is private, so an empty network cannot be built from
    /// outside the crate at all.
    #[test]
    fn an_empty_network_has_no_bank() {
        let empty = ChannelNetwork {
            polylines: Vec::new(),
            band_edges: Vec::new(),
            run_cells: Vec::new(),
            meander: SphereFbm::new(
                Seed(42).derive(streams::CHANNEL_MEANDER),
                MEANDER_FREQUENCY,
                MEANDER_OCTAVES,
            ),
        };
        assert_eq!(empty.bank_signed_distance(unit(1.0, 0.0, 0.0)), None);
    }

    /// The bank distance is the same number `nearest_line` reports, so a
    /// consumer that needs the line index and one that needs only the distance
    /// can never disagree about which bank a point is on.
    #[test]
    fn the_bank_distance_agrees_with_the_nearest_line() {
        let net = test_network();
        for p in [sample_left(&net), sample_right(&net)] {
            let (_, expected) = net.nearest_line(p).expect("the test network is non-empty");
            assert_eq!(net.bank_signed_distance(p), Some(expected));
        }
        assert!(net.bank_signed_distance(sample_left(&net)).unwrap() > 0.0);
        assert!(net.bank_signed_distance(sample_right(&net)).unwrap() < 0.0);
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
            run_cells: vec![(0..points.len() as u32).map(CellId).collect()],
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

    /// claim: structural(seed: [42, 7]) — exact vertex coincidence at every
    /// confluence of two fixed worlds, not a search across seeds for one that
    /// exhibits it. The second seed is coverage, not a hunt: the assertion is
    /// universal over every join in both worlds and the floor below asserts
    /// the population it ran on.
    ///
    /// THE CONFLUENCE REPAIR, as a property. Where a run ends on a cell some
    /// other run continues past, the two polylines must meet **exactly** —
    /// not merely nearby. Before the repair the tributary's mouth sat at the
    /// cell's undisplaced position and the trunk's copy was meander-displaced,
    /// a median 4.5 channel half-widths apart, which is what falsified H2.
    ///
    /// Exact equality is the right assertion because the repair is an
    /// assignment, not an approximation: a tolerance would pass for a network
    /// that merely brought the two ends close, which is the state this repair
    /// replaces.
    ///
    /// **The floor is the anti-vacuity half, and it is a floor rather than
    /// `> 0` for a measured reason.** Seed 42 at level 5 has exactly ONE
    /// confluence — and so do seeds 42, 7 and 1234 *together* on that grid —
    /// so a `joins > 0` assertion there would be satisfied by a single join
    /// and would stay green through a regression that stopped emitting almost
    /// every tributary. At level 6 the two campaign seeds carry
    /// [`CONFLUENCES_AT_LEVEL_6`] of them.
    ///
    /// Requiring **at least half** rather than the exact count is deliberate:
    /// the claim being guarded is "this assertion ran over a real population",
    /// which an exact count would turn into an unrelated tripwire reddening
    /// on any ordinary terrain drift.
    #[test]
    fn a_tributary_mouth_sits_exactly_on_the_trunk_vertex_it_joins() {
        let geo = Geosphere::new(6);
        let mut joins = 0usize;
        for seed in [42u64, 7] {
            let outcome =
                crate::globe::generate(Seed(seed), &geo, &crate::pins::TerrainPins::default())
                    .unwrap();
            let net =
                ChannelNetwork::build(&outcome.globe, &geo, outcome.globe.channel_noise_seed());
            // The same owner map `build` uses, rebuilt from the published
            // `run_cells` rather than from anything private.
            let mut owner: Vec<Option<(usize, usize)>> = vec![None; geo.cell_count()];
            for (i, run) in net.run_cells.iter().enumerate() {
                for (j, &c) in run.iter().enumerate() {
                    if j + 1 < run.len() {
                        owner[c.0 as usize] = Some((i, j));
                    }
                }
            }
            for (i, cells) in net.run_cells.iter().enumerate() {
                let last = cells.len() - 1;
                let Some((trunk, vertex)) = owner[cells[last].0 as usize] else {
                    continue;
                };
                if trunk == i {
                    continue;
                }
                joins += 1;
                assert_eq!(
                    net.polylines[i].points[last], net.polylines[trunk].points[vertex],
                    "seed {seed}: tributary {i} does not meet trunk {trunk} at cell {:?}",
                    cells[last]
                );
            }
        }
        assert!(
            joins * 2 >= CONFLUENCES_AT_LEVEL_6,
            "only {joins} confluences across the two seeds (measured \
             {CONFLUENCES_AT_LEVEL_6}) — the equality assertion above is running on almost \
             nothing"
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
        // The real production seed: the globe's own already-derived
        // CHANNEL_MEANDER leg, exactly as `GeneratedTerrain::new` passes it.
        let meander_seed = outcome.globe.channel_noise_seed();
        let net = ChannelNetwork::build(&outcome.globe, &geo, meander_seed);
        let again = ChannelNetwork::build(&outcome.globe, &geo, meander_seed);
        assert_eq!(net.polylines, again.polylines, "build is not deterministic");
        assert_eq!(net.band_edges, again.band_edges);
        assert!(
            !net.polylines.is_empty(),
            "no channels on a level-5 seed 42"
        );
        assert_eq!(net.run_cells.len(), net.polylines.len());
        for (line, cells) in net.polylines.iter().zip(&net.run_cells) {
            assert_eq!(
                line.points.len(),
                cells.len(),
                "run cells parallel to points"
            );
            // A run follows `downhill`, so consecutive cells are adjacent and
            // strictly descending — the property that makes a polyline a
            // rendering OF that run rather than an unrelated line beside it.
            for pair in cells.windows(2) {
                assert_eq!(
                    *outcome.globe.downhill.get(pair[0]),
                    Some(pair[1]),
                    "run cells are not a downhill chain"
                );
            }
        }
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
