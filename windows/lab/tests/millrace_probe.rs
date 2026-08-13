//! The Millrace, Task 2: what a capped nearest-line query would actually
//! gather, measured **before** any index exists.
//!
//! Spec §3.2 reduces the correctness of a bucketed `nearest_line` to one
//! inequality — every segment within `D` of `p` has an endpoint inside the cap
//! of radius `rho = D + L_max/2` — and spec §3.3 requires the *benefit* of
//! that cap measured first, on the shipped tree, with no index built. This
//! file is that measurement. It builds no index and changes no metric; it
//! replays the real query population and reports, per query, how many distinct
//! polylines the cap would have handed the scan.
//!
//! ```text
//! cargo test --release -p hornvale-lab --test millrace_probe -- --ignored --nocapture
//! ```
//!
//! # Two things this file is careful about
//!
//! **The query population is replayed, not invented.** `k` is a property of
//! the queries actually made, so both query generators here are transcriptions
//! of the shipped metric code — `lab_band_transects`' strided transect sweep
//! and `lab_channel_connectivity`'s seven interpolated join probes, including
//! the latter's early `break` on the first probe that leaves `Channel`, which
//! decides how many queries a walk issues at all. The lab's own helpers
//! (`lab_left_normal`, `lab_offset`, `lab_normalize`, `lab_run_owner`) and its
//! two sweep constants are private to `windows/lab/src/metrics.rs`, so they
//! are reproduced here verbatim rather than reached for. That duplication is
//! the cost of measuring from outside the crate, and it is the one thing in
//! this file that can silently rot: if either metric's sampling design moves,
//! this probe is measuring a population that no longer exists.
//!
//! **The instrument carries its own positive control.** A cap that is too
//! small produces a flatteringly tiny candidate set that simply does not
//! contain the answer, so every single query re-runs `nearest_line`'s exact
//! argmin over the candidate set alone and asserts it reproduces the full
//! scan's winner and its bit-identical signed distance
//! ([`shrink_factor`]). Spec §3.2's inequality is therefore checked, not
//! assumed, on every query this probe reports a `k` for.
//!
//! **A mean is not reported alone.** The win is structurally heterogeneous —
//! the cap's radius scales with the answer distance, so a probe beside a
//! headwater excludes almost everything and a probe far from any river
//! degrades toward the full scan. Everything below is a distribution.
//!
//! # `k`, and which tail the prediction is about
//!
//! `k = L / |candidates|`, `L` the polyline count, so **larger is better**.
//! Spec §2.1's P2 asks for `median k >= 8` and `95th-percentile-worst k >= 2`.
//! The worst queries are the ones with the *smallest* `k`, so the second
//! clause is a floor on the **5th percentile of `k`** (equivalently: on 95% of
//! queries the candidate set is at most half the network). Both are printed.

use hornvale_astronomy::SkyPins;
use hornvale_kernel::{CellId, Geosphere, Seed, SphericalPolyline, math};
use hornvale_lab::TerrainView;
use hornvale_terrain::{
    GeneratedTerrain, WaterKind,
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

/// Build the seed-42 world to `BuildDepth::Terrain` and hand back its terrain.
///
/// `BuildDepth::Terrain` is what the lab's channel metrics themselves run at
/// (every channel metric is an `Extractor::Terrain`), so this is the same
/// world the read path being instrumented sees.
fn probe_world() -> GeneratedTerrain {
    let view = TerrainView::build(Seed(SEED), &SkyPins::default())
        .expect("seed 42 builds to the terrain rung");
    assert_eq!(
        view.terrain.geosphere().level(),
        LEVEL,
        "this probe is only meaningful on the canonical grid"
    );
    view.terrain
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

/// `E_max` — the longest cell-to-neighbour edge on the mesh.
///
/// `NearestCellIndex::cover_deg` is private and `Geosphere` exposes no edge
/// accessor (`domains/terrain/tests/channel_properties.rs:29-33` says so), so
/// this walks `geo.neighbors` itself.
fn max_cell_edge(geo: &Geosphere) -> f64 {
    let mut worst = 0.0_f64;
    for id in 0..geo.cell_count() {
        let c = CellId(id as u32);
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
/// is the true answer distance the shipped linear scan returns.
///
/// The cap is applied as `dot >= cos(rho)` rather than `angle <= rho` — the
/// same test, one `acos` per query instead of one per vertex. `rho >= pi`
/// degenerates to `cos_rho = -1`, which every unit vector satisfies, so the
/// full-scan fallback needs no special case here.
///
/// `None` only when the network is empty, which `nearest_line` also answers
/// `None` for.
fn candidates(net: &ChannelNetwork, q: [f64; 3], half_max_segment: f64) -> Option<Vec<usize>> {
    let (_, signed) = net.nearest_line(q)?;
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
/// every query re-runs `nearest_line`'s exact argmin — `d.abs() <
/// best.abs()`, ascending polyline index — over the candidate set alone and
/// asserts it reproduces the full scan's winner *and* its bit-identical signed
/// distance. That is spec §3.2's inequality checked empirically, per query,
/// rather than trusted; a red here says the measurement below is worthless.
fn shrink_factor(net: &ChannelNetwork, q: [f64; 3], half_max_segment: f64) -> Option<f64> {
    let (winner, signed) = net.nearest_line(q)?;
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

/// Every position `lab_band_transects` issues a `bank_reading` for, in issue
/// order — a transcription of that function's sampling design with the
/// band-counting removed.
///
/// The stride, the `outer <= 0.0` skip, both sides and the inclusive
/// `0..=TRANSECT_STEPS` sweep are reproduced exactly; `s = 0` lands on the
/// vertex itself for both sides, and that duplicate is a real duplicate query
/// in the shipped sweep, so it is kept.
fn band_transect_queries(net: &ChannelNetwork) -> Vec<[f64; 3]> {
    let vertices: usize = net.polylines.iter().map(|l| l.points.len()).sum();
    if vertices == 0 {
        return Vec::new();
    }
    let stride = vertices.div_ceil(MAX_TRANSECTS).max(1);
    let mut out = Vec::new();
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
                for s in 0..=TRANSECT_STEPS {
                    let offset = side * outer * s as f64 / TRANSECT_STEPS as f64;
                    out.push(offset_from(line, j, left, offset));
                }
            }
        }
    }
    out
}

// ---------------------------------------------------------------------------
// Query population 2: `lab_channel_connectivity`'s join probes
// ---------------------------------------------------------------------------

/// The number of interpolated probes a walk issues at one join —
/// `for s in 1..8` in the shipped metric.
const JOIN_PROBES: usize = 7;

/// For each cell, the `(line, vertex)` of the run that CLAIMED it — a
/// transcription of `lab_run_owner`.
fn run_owner(net: &ChannelNetwork, cell_count: usize) -> Vec<Option<(usize, usize)>> {
    let mut owner = vec![None; cell_count];
    for (i, run) in net.run_cells.iter().enumerate() {
        for (j, &c) in run.iter().enumerate() {
            if j + 1 < run.len() {
                owner[c.0 as usize] = Some((i, j));
            }
        }
    }
    owner
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
    continues: &dyn Fn(CellId) -> bool,
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
    let terrain = probe_world();
    let net = terrain.channels();
    let l_max = max_segment_arc(net);
    let e_max = max_cell_edge(terrain.geosphere());
    println!(
        "L_max = {l_max:.9} rad, E_max = {e_max:.9} rad, ratio = {:.6}",
        l_max / e_max
    );
    assert!(
        l_max <= SEGMENT_CEILING_RATIO * e_max,
        "L_max {l_max} exceeds {SEGMENT_CEILING_RATIO} * E_max {e_max}: spec §3.2's \
         segment-length argument is wrong somewhere and the index must not be built"
    );
}

#[test]
#[ignore = "probe: replays ~50k real nearest-line queries against the full 3,606-line network (17 s measured); run by hand"]
fn the_candidate_set_a_capped_query_would_gather() {
    let terrain = probe_world();
    let net = terrain.channels();
    let globe = terrain.globe();
    let lines = net.polylines.len();
    let vertices: usize = net.polylines.iter().map(|l| l.points.len()).sum();
    let l_max = max_segment_arc(net);
    let half = l_max / 2.0;
    println!("network: {lines} polylines, {vertices} vertices; L_max = {l_max:.9} rad");

    // --- population 1: the transect sweep (predicate-independent) ----------
    let transects = band_transect_queries(net);
    let mut transect_k: Vec<f64> = transects
        .iter()
        .map(|&q| shrink_factor(net, q, half).expect("the network is not empty"))
        .collect();
    print_distribution("band transects", &mut transect_k, lines);

    // --- population 2: the join probes, under both predicates --------------
    let owner = run_owner(net, terrain.geosphere().cell_count());
    let probes = join_probes(net, &owner, half);

    let shipped = |last_cell: CellId| match *globe.downhill.get(last_cell) {
        Some(next) => matches!(*globe.water_kind.get(next), WaterKind::River),
        None => false,
    };
    // The repair `lab_channel_connectivity`'s own doc names: test whether a run
    // claims this cell, rather than the water class of the cell downstream of
    // it. A run claims a cell only where it continues past it, so this is the
    // direct question the water-class test was standing in for.
    let repaired = |last_cell: CellId| owner[last_cell.0 as usize].is_some();

    for (label, predicate) in [
        (
            "shipped WaterKind::River",
            &shipped as &dyn Fn(CellId) -> bool,
        ),
        (
            "repaired owner.is_some()",
            &repaired as &dyn Fn(CellId) -> bool,
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
