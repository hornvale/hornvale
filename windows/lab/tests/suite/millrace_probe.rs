//! The Millrace, Task 2: what a capped nearest-line query would actually
//! gather, measured against the **unindexed** linear scan.
//!
//! Spec §3.2 reduces the correctness of a bucketed `nearest_line` to one
//! inequality — every segment within `D` of `p` has an endpoint inside the cap
//! of radius `rho = D + L_max/2` — and spec §3.3 requires the *benefit* of
//! that cap measured first, on the shipped tree, before the index is built.
//! This file is that measurement. It builds no index of its own and changes no
//! metric; it replays the real query population and reports, per query, how
//! many distinct polylines the cap would have handed the scan.
//!
//! ```text
//! cargo test --release -p hornvale-lab --test suite -- millrace_probe --ignored --nocapture
//! ```
//!
//! # The oracle is the reference scan, deliberately
//!
//! Task 2 wrote this file when `ChannelNetwork::nearest_line` **was** the
//! linear scan, so taking `D` and the winner from it made the control below
//! independent by construction. Task 4 then made `nearest_line` the bucket
//! index, and that independence silently lapsed: the probe's cap and the
//! index's gather would have rested on the *same* coverage inequality with the
//! *same* `L_max`, so a wrong inequality would be **agreed on rather than
//! caught**. Every `D`, winner and signed distance here therefore comes from
//! `ChannelNetwork::nearest_line_reference_for_test` — the unindexed scan the
//! index replaced, which `domains/terrain/tests/channel_properties.rs`
//! separately pins the index equal to.
//!
//! **What restoring it costs, measured rather than guessed.** Every figure
//! this file prints is bit-identical either way (as the equality property
//! battery says it must be), but the measurement test went **1.1 s -> 23.1 s**
//! (`--release`, seed 42, ~41k queries): the reference scan's per-polyline
//! segment projection is far heavier than the cap sweep's early-exiting dot
//! products, so the independent oracle is roughly 20x the indexed one. This is
//! an `#[ignore]`d hand-run probe, so that buys a control that can actually
//! disagree with the index for a cost nothing in the gate pays.
//!
//! # Four things this file is careful about
//!
//! **The query population is replayed, not invented.** `k` is a property of
//! the queries actually made, so both query generators here are transcriptions
//! of the shipped metric code — `lab_band_transects`' strided transect sweep
//! and `lab_channel_connectivity`'s seven interpolated join probes, including
//! the latter's early `break` on the first probe that leaves `Channel`, which
//! decides how many queries a walk issues at all. The lab's own helpers
//! (`lab_left_normal`, `lab_offset`, `lab_normalize`, `lab_run_owner`) and its
//! two sweep constants are private to `windows/lab/src/metrics.rs`, so they
//! are reproduced here verbatim rather than reached for.
//!
//! **That duplication is guarded, on the transect half only.**
//! [`assert_sweep_matches_published`] recomputes
//! `channel-band-monotonicity-untruncated` and `channel-transect-dry-reach`
//! from the transcribed sweep and asserts bit-equality with the shipped
//! registry's own extractors, so a drift in stride, window, step count or
//! ordering turns this file red. **The join half has no such guard and cannot
//! get one from published output**: `channel-connectivity` is the only metric
//! reading that population and `lab_channel_connectivity`'s own doc records it
//! as constant since the confluence repair, so it reads 1.0000 for any
//! transcription including a badly wrong one. Two corroborations an earlier
//! draft of this file offered — that connectivity reproduces at 1.0000, and
//! that the network measures 3,606 / 14,606 — are worth nothing for exactly
//! that reason: the first is a documented constant and the second corroborates
//! the world build.
//!
//! **Since Task 5 the join half's *findings* are no longer taken here.** The
//! vacuity measurement (P3) and the two-arm comparison that decided whether the
//! repair moved a census value live in-crate, in `metrics.rs`'s own test module
//! (`the_connectivity_arms_over_the_ford_probe_seeds`), where the shipped walk
//! can be called directly under either continuation rule instead of
//! transcribed. What survives here is the `k` measurement, which needs the
//! query *positions* and not the verdicts.
//!
//! **The instrument carries its own positive control.** A cap that is too
//! small produces a flatteringly tiny candidate set that simply does not
//! contain the answer, so every single query re-runs the exact argmin over the
//! candidate set alone and asserts it reproduces the **reference scan's**
//! winner and its bit-identical signed distance ([`shrink_factor`]). Spec
//! §3.2's inequality is therefore checked, not assumed, on every query this
//! probe reports a `k` for.
//!
//! **A mean is not reported alone.** The win is structurally heterogeneous —
//! the cap's radius scales with the answer distance, so a probe beside a
//! headwater excludes almost everything and a probe far from any river
//! degrades toward the full scan. Everything below is a distribution.
//!
//! # `k` here is an UPPER BOUND on the `k` an index delivers
//!
//! `k = L / |candidates|`, `L` the polyline count, so **larger is better**.
//! What this file computes is the *cap's* candidate set, using the true answer
//! distance `D` from a **completed** full scan. A real index is worse than
//! that in two one-directional ways, and neither is a defect in the design:
//!
//! 1. It cannot know `D` before it has candidates, so spec §3.2's loop always
//!    gathers at a radius **≥** the one used here.
//! 2. It gathers by **bucket**, not by cap. `L_max/2` is about 0.58 mean
//!    level-6 cell spacings, so `rho` is sub-cell for every near-channel
//!    query, and any bucket grid hands back a 2x2-to-3x3 neighbourhood —
//!    4-9x the cap's area.
//!
//! So a delivered median candidate set of ~4-9 lines is the realistic reading
//! of a measured 1-2. Nothing in the verdict moves (that is still 50-110x
//! P2's floor of 8), but no delivered factor should be quoted from this
//! file's raw numbers.
//!
//! Spec §2.1's P2 asks for `median k >= 8` and `95th-percentile-worst k >= 2`.
//! The worst queries are the ones with the *smallest* `k`, so the second
//! clause is a floor on the **5th percentile of `k`** (equivalently: on 95% of
//! queries the candidate set is at most half the network). Both are printed.

use hornvale_astronomy::SkyPins;
use hornvale_kernel::{Geosphere, Seed, SphericalPolyline, Vertex, math};
use hornvale_lab::{Extractor, MetricValue, TerrainView, registry};
use hornvale_terrain::{
    WaterKind,
    channel::{ChannelNetwork, Transverse},
};

/// The seed every measurement here is taken on — the project's canonical
/// world, and the one spec §1.4's 3,606-line / 14,606-vertex figures are for.
const SEED: u64 = 42;

/// The canonical grid (`hornvale_terrain::GLOBE_LEVEL`). Level-5 figures
/// scattered through test comments understate this network ~4x, so the level
/// is asserted rather than assumed.
const LEVEL: u32 = 6;

/// `LAB_FORD_TRANSECT_STEPS`, reproduced from `windows/lab/src/metrics.rs`.
const TRANSECT_STEPS: usize = 48;

/// `LAB_FORD_MAX_TRANSECTS`, reproduced from `windows/lab/src/metrics.rs`.
const MAX_TRANSECTS: usize = 256;

/// Spec §3.2's theoretical ceiling on the longest channel segment, as a
/// multiple of the mesh's longest cell-to-neighbour edge.
const SEGMENT_CEILING_RATIO: f64 = 1.5;

// ---------------------------------------------------------------------------
// The world under measurement
// ---------------------------------------------------------------------------

/// Build the seed-42 world to `BuildDepth::Terrain`.
///
/// `BuildDepth::Terrain` is what the lab's channel metrics themselves run at
/// (every channel metric is an `Extractor::Terrain`), so this is the same
/// world the read path being instrumented sees. The whole `TerrainView` is
/// returned rather than just its terrain, because the drift guard needs to
/// hand it to the registry's own extractors.
fn probe_world() -> TerrainView {
    let view = TerrainView::build(Seed(SEED), &SkyPins::default())
        .expect("seed 42 builds to the terrain rung");
    assert_eq!(
        view.terrain.geosphere().level(),
        LEVEL,
        "this probe is only meaningful on the canonical grid"
    );
    view
}

// ---------------------------------------------------------------------------
// Geometry — transcribed from the lab's private helpers
// ---------------------------------------------------------------------------

/// Dot product of two vectors.
fn dot(a: [f64; 3], b: [f64; 3]) -> f64 {
    a[0] * b[0] + a[1] * b[1] + a[2] * b[2]
}

/// Cross product of two vectors.
fn cross(a: [f64; 3], b: [f64; 3]) -> [f64; 3] {
    [
        a[1] * b[2] - a[2] * b[1],
        a[2] * b[0] - a[0] * b[2],
        a[0] * b[1] - a[1] * b[0],
    ]
}

/// Scale a vector to unit length, leaving a zero vector alone.
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

/// The unit left-normal at vertex `j` — the direction a transect walks.
fn left_normal(line: &SphericalPolyline, j: usize) -> [f64; 3] {
    let n = line.points.len();
    let a = line.points[j.saturating_sub(1)];
    let b = line.points[(j + 1).min(n - 1)];
    let tangent = normalize([b[0] - a[0], b[1] - a[1], b[2] - a[2]]);
    normalize(cross(line.points[j], tangent))
}

/// `offset` radians to the left of vertex `j`, on the unit sphere.
fn offset_from(line: &SphericalPolyline, j: usize, left: [f64; 3], offset: f64) -> [f64; 3] {
    let p = line.points[j];
    normalize([
        p[0] + left[0] * offset,
        p[1] + left[1] * offset,
        p[2] + left[2] * offset,
    ])
}

// ---------------------------------------------------------------------------
// The two bounded quantities
// ---------------------------------------------------------------------------

/// `L_max` — the longest arc between consecutive vertices of any polyline.
///
/// This is the quantity spec §3.2 says an index must measure in one O(V) pass
/// at build time rather than assume.
fn max_segment_arc(net: &ChannelNetwork) -> f64 {
    let mut worst = 0.0_f64;
    for line in &net.polylines {
        for pair in line.points.windows(2) {
            worst = worst.max(angle(pair[0], pair[1]));
        }
    }
    worst
}

/// The mean cell-to-neighbour edge on the mesh — the length scale that decides
/// whether a cap of radius `rho` is sub-cell.
///
/// This is the load-bearing number for how far the measured `k` can be
/// trusted: `rho = D + L_max/2`, and if `L_max/2` is well under one cell
/// spacing then for any near-channel query the cap cannot reach past the
/// query's own cell and its immediate surroundings. It is measured here rather
/// than quoted, because everything the report says about the bound between the
/// measured `k` and a delivered one rests on it.
fn mean_cell_edge(geo: &Geosphere) -> f64 {
    let mut total = 0.0;
    let mut count = 0usize;
    for id in 0..geo.vertex_count() {
        let c = Vertex(id as u32);
        let p = geo.position(c);
        for &n in geo.neighbors(c) {
            total += angle(p, geo.position(n));
            count += 1;
        }
    }
    total / count as f64
}

/// `E_max` — the longest cell-to-neighbour edge on the mesh.
///
/// `NearestVertexIndex::cover_deg` is private and `Geosphere` exposes no edge
/// accessor (`domains/terrain/tests/channel_properties.rs:29-33` says so), so
/// this walks `geo.neighbors` itself.
fn max_cell_edge(geo: &Geosphere) -> f64 {
    let mut worst = 0.0_f64;
    for id in 0..geo.vertex_count() {
        let c = Vertex(id as u32);
        let p = geo.position(c);
        for &n in geo.neighbors(c) {
            worst = worst.max(angle(p, geo.position(n)));
        }
    }
    worst
}

// ---------------------------------------------------------------------------
// The instrument: what a cap of radius D + L_max/2 would have gathered
// ---------------------------------------------------------------------------

/// The candidate set a capped query at `q` would gather: the **distinct
/// polylines with a vertex inside the cap** of radius `D + L_max/2`, where `D`
/// is the true answer distance the **unindexed** linear scan
/// (`ChannelNetwork::nearest_line_reference_for_test`) returns — never the
/// index's, for the reason the module doc gives.
///
/// The cap is applied as `dot >= cos(rho)` rather than `angle <= rho` — the
/// same test, one `acos` per query instead of one per vertex. `rho >= pi`
/// degenerates to `cos_rho = -1`, which every unit vector satisfies, so the
/// full-scan fallback needs no special case here.
///
/// `None` only when the network is empty, which the reference scan also
/// answers `None` for.
fn candidates(net: &ChannelNetwork, q: [f64; 3], half_max_segment: f64) -> Option<Vec<usize>> {
    let (_, signed) = net.nearest_line_reference_for_test(q)?;
    let rho = signed.abs() + half_max_segment;
    let cos_rho = if rho >= std::f64::consts::PI {
        -1.0
    } else {
        math::cos(rho)
    };
    Some(
        net.polylines
            .iter()
            .enumerate()
            .filter(|(_, line)| line.points.iter().any(|p| dot(q, *p) >= cos_rho))
            .map(|(i, _)| i)
            .collect(),
    )
}

/// `k = L / |candidates|` for one query — larger is better.
///
/// **This is also the instrument's positive control, and it is not optional.**
/// A units error, a stale `L_max`, or a sign slip in the cap would all show up
/// as an impressively small candidate set that simply does not contain the
/// answer, and the resulting `k` would be a large, meaningless number. So
/// every query re-runs the exact argmin — `d.abs() < best.abs()`, ascending
/// polyline index — over the candidate set alone and asserts it reproduces the
/// **unindexed reference scan's** winner *and* its bit-identical signed
/// distance. That is spec §3.2's inequality checked empirically, per query,
/// rather than trusted; a red here says the measurement below is worthless.
///
/// The comparison is against the reference scan and not against
/// `ChannelNetwork::nearest_line`, because since Task 4 the latter gathers by
/// bucket under the *same* coverage inequality with the *same* `L_max`: a
/// wrong inequality would be agreed on by both sides rather than caught.
fn shrink_factor(net: &ChannelNetwork, q: [f64; 3], half_max_segment: f64) -> Option<f64> {
    let (winner, signed) = net.nearest_line_reference_for_test(q)?;
    let gathered = candidates(net, q, half_max_segment)?;
    let mut best = f64::INFINITY;
    let mut best_line: Option<usize> = None;
    for &i in &gathered {
        let d = net.polylines[i].signed_distance(q);
        if d.abs() < best.abs() {
            best = d;
            best_line = Some(i);
        }
    }
    assert_eq!(
        best_line,
        Some(winner),
        "the cap of radius D + L_max/2 = {} did not contain the true winner at {q:?}: \
         spec §3.2's inequality does not hold as implemented, and every k below is meaningless",
        signed.abs() + half_max_segment
    );
    assert_eq!(
        best.to_bits(),
        signed.to_bits(),
        "the capped scan returned a different signed distance than the full scan at {q:?}"
    );
    Some(net.polylines.len() as f64 / gathered.len() as f64)
}

// ---------------------------------------------------------------------------
// Query population 1: `lab_band_transects`' strided transect sweep
// ---------------------------------------------------------------------------

/// The transcribed sweep: every position `lab_band_transects` issues a
/// `bank_reading` for, in issue order, **plus the two counts that make the
/// transcription checkable against published output**.
struct TranscribedSweep {
    /// Positions queried, in issue order.
    queries: Vec<[f64; 3]>,
    /// Transects swept — the denominator of both published ratios.
    transects: usize,
    /// Transects monotone over the whole sweep, truncating nothing.
    monotone_untruncated: usize,
    /// Own-channel prefixes that reached `Dry` before truncation.
    reached_dry: usize,
}

/// Transcribe `lab_band_transects` — its sampling design *and* enough of its
/// band-counting to reproduce two published metric values.
///
/// The stride, the `outer <= 0.0` skip, both sides and the inclusive
/// `0..=TRANSECT_STEPS` sweep are reproduced exactly; `s = 0` lands on the
/// vertex itself for both sides, and that duplicate is a real duplicate query
/// in the shipped sweep, so it is kept.
///
/// **Why the counting is here at all, when only the positions are measured.**
/// A transcription that drifts from the function it copies is the one failure
/// a probe like this cannot afford, and the first version of this file offered
/// two corroborations that are both vacuous: `channel-connectivity` reads
/// 1.0000 for *any* walk transcription (`lab_channel_connectivity`'s own doc
/// in `windows/lab/src/metrics.rs` records that the metric is constant since
/// the confluence repair and that both of its failure branches are
/// unreachable), and the network's 3,606 / 14,606 dimensions
/// corroborate the world build rather than the sweep. Carrying the counts lets
/// [`assert_sweep_matches_published`] compare against real published output
/// instead, which moves if the stride, the window, the step count or the
/// iteration order moves.
fn transcribed_band_sweep(net: &ChannelNetwork) -> TranscribedSweep {
    let mut out = TranscribedSweep {
        queries: Vec::new(),
        transects: 0,
        monotone_untruncated: 0,
        reached_dry: 0,
    };
    let vertices: usize = net.polylines.iter().map(|l| l.points.len()).sum();
    if vertices == 0 {
        return out;
    }
    let stride = vertices.div_ceil(MAX_TRANSECTS).max(1);
    let mut index = 0usize;
    for (i, line) in net.polylines.iter().enumerate() {
        for j in 0..line.points.len() {
            let take = index.is_multiple_of(stride);
            index += 1;
            if !take {
                continue;
            }
            let outer = net.band_edges[i][j][3] * 1.5;
            if outer <= 0.0 {
                continue;
            }
            let left = left_normal(line, j);
            for side in [1.0_f64, -1.0] {
                out.transects += 1;
                let mut previous = 0u8;
                let mut still_own = true;
                let mut good_untruncated = true;
                let mut reached_dry = false;
                for s in 0..=TRANSECT_STEPS {
                    let offset = side * outer * s as f64 / TRANSECT_STEPS as f64;
                    let q = offset_from(line, j, left, offset);
                    out.queries.push(q);
                    let (owns, band) = match net.bank_reading(q) {
                        Some(reading) => (reading.line == i, reading.transverse().index()),
                        None => (false, Transverse::Dry.index()),
                    };
                    if !owns {
                        still_own = false;
                    }
                    if band < previous {
                        good_untruncated = false;
                    }
                    previous = band;
                    // The shipped loop also tracks `own_previous` here to
                    // score `channel-band-monotonicity`. That column is not
                    // reproduced: `lab_band_transects`' own doc records it
                    // reading 1.0 on all 64 probe worlds, so asserting on it
                    // would corroborate nothing. `reached_dry` is the half of
                    // this branch that still discriminates.
                    if still_own && band == Transverse::Dry.index() {
                        reached_dry = true;
                    }
                }
                if good_untruncated {
                    out.monotone_untruncated += 1;
                }
                if reached_dry {
                    out.reached_dry += 1;
                }
            }
        }
    }
    out
}

/// The published value of one registry metric, read through the public
/// `registry()` / `Metric.extract` surface.
fn published_number(view: &TerrainView, name: &str) -> f64 {
    let metric = registry()
        .into_iter()
        .find(|m| m.name == name)
        .unwrap_or_else(|| panic!("no metric named {name} in the registry"));
    let Extractor::Terrain(extract) = metric.extract else {
        panic!("{name} is not a Terrain-rung metric");
    };
    match extract(view) {
        MetricValue::Number(x) => x,
        other => panic!("{name} did not read as a number: {other:?}"),
    }
}

/// **The drift guard on the transcription.** Recompute two published metrics
/// from the transcribed sweep and assert bit-equality with the shipped
/// registry's own extractors.
///
/// The two chosen are `channel-band-monotonicity-untruncated` and
/// `channel-transect-dry-reach`, both `monotone_untruncated / transects` and
/// `reached_dry / transects` over exactly the population this probe replays.
/// `channel-band-monotonicity` is deliberately **not** used:
/// `lab_band_transects`' own doc in `windows/lab/src/metrics.rs` records it
/// reading 1.0 on all 64 probe worlds, which makes it exactly as vacuous a
/// corroboration as `channel-connectivity`.
///
/// **What this does and does not cover.** It covers the transect half — a
/// drift in the stride, the outward window, the step count, the side loop or
/// the iteration order moves at least one of these two ratios. It covers the
/// join half **not at all**, and no available published observable would:
/// `channel-connectivity` is the only metric reading that population and it is
/// a documented constant. That gap is stated rather than papered over. Task 5
/// took the durable fix for everything that *depends* on the join half — the
/// vacuity measurement and the two-arm comparison moved in-crate, where the
/// shipped walk is called rather than transcribed — but this file's own join
/// transcription, which exists only to source query positions, still has no
/// guard.
fn assert_sweep_matches_published(view: &TerrainView, sweep: &TranscribedSweep) {
    let cases = [
        (
            "channel-band-monotonicity-untruncated",
            sweep.monotone_untruncated as f64 / sweep.transects as f64,
        ),
        (
            "channel-transect-dry-reach",
            sweep.reached_dry as f64 / sweep.transects as f64,
        ),
    ];
    for (name, mine) in cases {
        let theirs = published_number(view, name);
        assert_eq!(
            mine.to_bits(),
            theirs.to_bits(),
            "the transcribed transect sweep no longer reproduces {name} ({mine} vs the \
             published {theirs}). `lab_band_transects`' sampling design has moved, so the \
             query population this probe replays is not the one the census issues, and \
             every k it reports is about a population that no longer exists"
        );
        println!("  drift guard: {name} = {theirs} reproduced from the transcribed sweep");
    }
}

// ---------------------------------------------------------------------------
// Query population 2: `lab_channel_connectivity`'s join probes
// ---------------------------------------------------------------------------

/// The number of interpolated probes a walk issues at one join —
/// `for s in 1..8` in the shipped metric.
const JOIN_PROBES: usize = 7;

/// For each cell, the `(line, vertex)` of the run that carries it onward.
///
/// **Read from the network's published `trunk_vertex` index, not rebuilt.**
/// This was a transcription of the lab's private `lab_run_owner` until Task 5
/// made the metric itself read the published accessor; the two rebuilds are
/// not identical (one keeps the first claiming run, the other the last), so
/// with the metric no longer rebuilding, neither does this.
fn run_owner(net: &ChannelNetwork, vertex_count: usize) -> Vec<Option<(usize, usize)>> {
    (0..vertex_count)
        .map(|i| net.trunk_vertex(Vertex(i as u32)))
        .collect()
}

/// One join probe, evaluated once and reused by every walk that crosses it.
///
/// Walks share trunk suffixes heavily — that sharing is exactly what the
/// campaign's L1 memoisation exploits — so the same seven positions are
/// re-queried by many walks. The `k` distribution is weighted by how many
/// times a query is *issued*, since that is what an index would actually pay,
/// but each distinct position is only *evaluated* once here.
struct JoinProbe {
    /// `k` for this position.
    k: f64,
    /// Whether the shipped predicate reads `Channel` here — the thing that
    /// decides whether the walk issues its remaining probes.
    channel: bool,
}

/// Per polyline, the trunk its walk hops onto and the seven probes that hop
/// issues — `None` where no run claims this run's last cell.
fn join_probes(
    net: &ChannelNetwork,
    owner: &[Option<(usize, usize)>],
    half_max_segment: f64,
) -> Vec<Option<Vec<JoinProbe>>> {
    let mut out = Vec::with_capacity(net.polylines.len());
    for line in 0..net.polylines.len() {
        let last_cell = *net.run_cells[line].last().expect("a run has cells");
        let Some((trunk, vertex)) = owner[last_cell.0 as usize] else {
            out.push(None);
            continue;
        };
        let from = *net.polylines[line].points.last().expect("a run has points");
        let to = net.polylines[trunk].points[vertex];
        let mut probes = Vec::with_capacity(JOIN_PROBES);
        for s in 1..=JOIN_PROBES {
            let t = s as f64 / 8.0;
            let q = normalize([
                from[0] + (to[0] - from[0]) * t,
                from[1] + (to[1] - from[1]) * t,
                from[2] + (to[2] - from[2]) * t,
            ]);
            probes.push(JoinProbe {
                k: shrink_factor(net, q, half_max_segment).expect("the network is not empty"),
                channel: net.transverse_at(q).0 == Transverse::Channel,
            });
        }
        out.push(Some(probes));
    }
    out
}

/// What one continuation predicate's walks issue.
struct WalkArm {
    /// `k` for every query issued, with multiplicity.
    k: Vec<f64>,
    /// Joins crossed, summed over all walks.
    hops: usize,
    /// Walks that reached the sea or a sink without leaving `Channel` — the
    /// numerator of `channel-connectivity` itself.
    intact: usize,
    /// Walks started: one per polyline.
    walks: usize,
}

/// Replay `lab_channel_connectivity`'s walks under `continues`, collecting the
/// `k` of every query they issue.
///
/// The control flow mirrors the shipped loop exactly, including the two
/// `break`s that stop a walk short: an unowned last cell, and the first probe
/// that reads anything but `Channel`. Those breaks are why the query
/// population is predicate-dependent and cannot be enumerated up front.
fn walk_arm(
    net: &ChannelNetwork,
    owner: &[Option<(usize, usize)>],
    probes: &[Option<Vec<JoinProbe>>],
    continues: &dyn Fn(Vertex) -> bool,
) -> WalkArm {
    let mut arm = WalkArm {
        k: Vec::new(),
        hops: 0,
        intact: 0,
        walks: 0,
    };
    for start in 0..net.polylines.len() {
        arm.walks += 1;
        let mut line = start;
        let mut good = true;
        for _ in 0..=net.polylines.len() {
            let last_cell = *net.run_cells[line].last().expect("a run has cells");
            if !continues(last_cell) {
                break; // reached the sea or a terminal sink
            }
            let Some((trunk, _)) = owner[last_cell.0 as usize] else {
                good = false;
                break; // a river cell no run owns: the walk falls out
            };
            arm.hops += 1;
            let hop = probes[line].as_ref().expect("owned last cell has probes");
            for probe in hop {
                arm.k.push(probe.k);
                if !probe.channel {
                    good = false;
                    break;
                }
            }
            if !good {
                break;
            }
            line = trunk;
        }
        if good {
            arm.intact += 1;
        }
    }
    arm
}

// ---------------------------------------------------------------------------
// Reporting
// ---------------------------------------------------------------------------

/// The nearest-rank value at percentile `p` (0-100) of an ascending slice.
fn percentile(sorted: &[f64], p: f64) -> f64 {
    assert!(!sorted.is_empty(), "no percentile of an empty sample");
    let rank = (p / 100.0 * sorted.len() as f64).ceil() as usize;
    sorted[rank.saturating_sub(1).min(sorted.len() - 1)]
}

/// Print the full `k` distribution for one query population, and the candidate
/// count each quantile corresponds to.
///
/// Sorts in place. The mean is printed last and labelled, never on its own —
/// spec §2.1 is explicit that a mean would hide the failure mode that matters.
fn print_distribution(label: &str, k: &mut [f64], lines: usize) {
    if k.is_empty() {
        println!("{label}: no queries");
        return;
    }
    k.sort_by(|a, b| a.total_cmp(b));
    let quantiles = [
        ("min", 0.0),
        ("p5", 5.0),
        ("p25", 25.0),
        ("median", 50.0),
        ("p75", 75.0),
        ("p95", 95.0),
        ("max", 100.0),
    ];
    println!("{label}: n = {}", k.len());
    println!("    {:>8}  {:>12}  {:>10}", "quantile", "k", "candidates");
    for (name, p) in quantiles {
        let v = if p == 0.0 { k[0] } else { percentile(k, p) };
        println!(
            "    {name:>8}  {v:>12.3}  {:>10.0}",
            lines as f64 / v.max(f64::MIN_POSITIVE)
        );
    }
    let mean = k.iter().sum::<f64>() / k.len() as f64;
    println!("    (mean k, reported only alongside the distribution: {mean:.3})");
}

/// Score one arm against spec §2.1's prediction P2 and print the verdict.
fn score_p2(label: &str, k: &mut [f64]) {
    k.sort_by(|a, b| a.total_cmp(b));
    let median = percentile(k, 50.0);
    let worst_95 = percentile(k, 5.0);
    println!(
        "P2 [{label}]: median k = {median:.3} (needs >= 8) -> {}; 95th-percentile-worst k = {worst_95:.3} (needs >= 2) -> {}",
        if median >= 8.0 { "HOLDS" } else { "FALSIFIED" },
        if worst_95 >= 2.0 {
            "HOLDS"
        } else {
            "FALSIFIED"
        },
    );
}

// ---------------------------------------------------------------------------
// The tests
// ---------------------------------------------------------------------------

#[test]
#[ignore = "probe: builds a seed-42 level-6 world and walks every mesh neighbour (0.2 s measured); run by hand"]
fn the_longest_channel_segment_stays_inside_the_mesh_edge_ceiling() {
    let view = probe_world();
    let net = view.terrain.channels();
    let l_max = max_segment_arc(net);
    let e_max = max_cell_edge(view.terrain.geosphere());
    let e_mean = mean_cell_edge(view.terrain.geosphere());
    println!(
        "L_max = {l_max:.9} rad, E_max = {e_max:.9} rad, ratio = {:.6}",
        l_max / e_max
    );
    // The bound between a measured k and a delivered one (see the module doc's
    // UPPER BOUND section) is exactly this ratio: a cap half-width well under
    // one cell spacing cannot reach past the query's own neighbourhood.
    println!(
        "E_mean = {e_mean:.9} rad; L_max/2 = {:.9} rad = {:.4} mean cell spacings",
        l_max / 2.0,
        l_max / 2.0 / e_mean
    );
    assert!(
        l_max <= SEGMENT_CEILING_RATIO * e_max,
        "L_max {l_max} exceeds {SEGMENT_CEILING_RATIO} * E_max {e_max}: spec §3.2's \
         segment-length argument is wrong somewhere and the index must not be built"
    );
}

/// The drift guard, standing on its own so it has a findable name and a
/// distinct failure. The measurement test below runs the same assertion on its
/// own sweep, so running that test alone is still guarded.
#[test]
#[ignore = "probe: sweeps every transect twice — once transcribed, once through the shipped metric (0.3 s measured, --release, post-Task-4 index); run by hand"]
fn the_transcribed_transect_sweep_reproduces_the_published_metrics() {
    let view = probe_world();
    let sweep = transcribed_band_sweep(view.terrain.channels());
    println!("transcribed sweep: {} transects", sweep.transects);
    assert_sweep_matches_published(&view, &sweep);
}

#[test]
#[ignore = "probe: replays ~50k real nearest-line queries against the full 3,606-line network, each answered by the UNINDEXED reference scan (23.1 s measured, --release; 1.1 s when the oracle was the Task-4 index, which is why the module doc explains the trade); run by hand"]
fn the_candidate_set_a_capped_query_would_gather() {
    let view = probe_world();
    let net = view.terrain.channels();
    let globe = view.terrain.globe();
    let lines = net.polylines.len();
    let vertices: usize = net.polylines.iter().map(|l| l.points.len()).sum();
    let l_max = max_segment_arc(net);
    let half = l_max / 2.0;
    println!("network: {lines} polylines, {vertices} vertices; L_max = {l_max:.9} rad");

    // --- population 1: the transect sweep (predicate-independent) ----------
    // The drift guard runs FIRST, before any k is reported: a transcription
    // that no longer reproduces the shipped sweep is measuring a population
    // the census does not issue.
    let sweep = transcribed_band_sweep(net);
    assert_sweep_matches_published(&view, &sweep);
    let mut transect_k: Vec<f64> = sweep
        .queries
        .iter()
        .map(|&q| shrink_factor(net, q, half).expect("the network is not empty"))
        .collect();
    print_distribution("band transects", &mut transect_k, lines);

    // --- population 2: the join probes, under both predicates --------------
    let owner = run_owner(net, view.terrain.geosphere().vertex_count());
    let probes = join_probes(net, &owner, half);

    // The rule this metric shipped until Task 5: continue while the cell
    // downstream classifies `River`. Kept as an arm because it is the
    // population every `k` figure before Task 5 was measured over.
    let superseded = |last_cell: Vertex| match *globe.downhill.get(last_cell) {
        Some(next) => matches!(*globe.water_kind.get(next), WaterKind::River),
        None => false,
    };
    // The rule the metric ships now: `ChannelNetwork::build`'s own reach
    // predicate, which is what decides whether a run continues past a cell.
    // Task 5 asserts in-crate that this agrees with "a run carries this cell"
    // on every run's last cell, on all 64 of these worlds.
    let shipped = |last_cell: Vertex| {
        !matches!(*globe.water_kind.get(last_cell), WaterKind::Ocean)
            && globe.downhill.get(last_cell).is_some()
    };

    for (label, predicate) in [
        (
            "superseded WaterKind::River",
            &superseded as &dyn Fn(Vertex) -> bool,
        ),
        (
            "shipped reach predicate",
            &shipped as &dyn Fn(Vertex) -> bool,
        ),
    ] {
        let mut arm = walk_arm(net, &owner, &probes, predicate);
        println!(
            "\n=== {label} === walks {}, joins crossed {}, queries {}, connectivity {:.4}",
            arm.walks,
            arm.hops,
            arm.k.len(),
            arm.intact as f64 / arm.walks as f64,
        );
        print_distribution("  join probes", &mut arm.k, lines);
        let mut combined = transect_k.clone();
        combined.extend_from_slice(&arm.k);
        print_distribution(
            "  WHOLE POPULATION (transects + joins)",
            &mut combined,
            lines,
        );
        score_p2(label, &mut combined);
    }
}
