//! The Rill, Task 4: the Tier 2 measurement instrument — containment and
//! channel area (R-6), and Horton's laws with their falsification (R-5).
//!
//! Both are **measurements, not gates**, so both are `#[ignore]`d under the
//! `probe:` token `rift_probe.rs` established. They enumerate millions of
//! branches; the cost is now measured rather than guessed at — **21.8 s** for
//! the containment sweep (104 MB peak RSS) and **1.4 s** for the Horton sweep
//! (158 MB), on the dev profile, which is optimized workspace-wide since
//! decision 0113. Both doc and ignore reasons said "minutes" before anything
//! had run them. Run them by hand:
//!
//! ```text
//! cargo test --release -p hornvale-terrain --test suite -- rill_probe --ignored --nocapture
//! ```
//!
//! Nothing here asserts a preregistered range. R-5's `[3.0, 5.0]` and
//! `[1.5, 3.5]` are the empirically observed ranges for **real river
//! networks** and R-6's `[0.005%, 0.5%]` is `channel-land-fraction`'s
//! preregistered interval — references outside this code, and the campaign's
//! instruction is to take the branch and report, never to tune the generator
//! until it lands inside them. The verdict is scored in the task report from
//! the numbers this file prints.
//!
//! # Two corrections this instrument carries, both from the falsified Tier 2
//!
//! **Containment means a polyline PASSES THROUGH the room.** The previous
//! probe reported 74% and the number was arithmetically identical to the
//! flow-room fraction of land — it would not have moved if the subdivision had
//! been deleted, because it counted rooms whose own flow query was `Some`.
//! Here containment is measured the way Task 3 measured its 1.237%: walk every
//! line at a fraction of a room's edge and collect the DISTINCT rooms the
//! samples land in.
//!
//! **Channel area integrates true arc lengths.** The previous probe used a
//! room's mean edge as the length of each reach where the correct figure is
//! centroid to centroid — two inradii, `spacing/√3` — and so overstated the
//! integral by exactly `√3`. That correction does not apply to this design at
//! all, and the reason is worth stating rather than silently dropping: this
//! network's lines are real polylines with real endpoints, so their lengths
//! are measured rather than imputed from a room's geometry. `spacing/√3` and
//! `spacing` were two guesses at a quantity that is now simply known, and
//! Task 3's own row 3 was computed the same way — by integrating the line.

use hornvale_kernel::{Facet, Geosphere, NearestVertexIndex, Seed, Vertex, math};
use hornvale_terrain::{
    CatchmentCut, ChannelNetwork, Rill, TectonicGlobe, TerrainPins, WaterKind, cell_catchment,
    channel_half_width, generate, rills_of, room_spacing,
};
use std::collections::{BTreeMap, BTreeSet};

/// The seeds every measurement here sweeps — the campaign's usual trio.
const SEEDS: [u64; 3] = [42, 7, 1234];

/// The canonical grid the coarse flow graph lives on.
const LEVEL: u32 = 6;

/// Walk depth: the room depth `windows/locale` places a walker at, and the
/// depth every containment number in this file is stated at.
const WALK_DEPTH: u32 = 12;

/// Coarse cells sampled per seed for containment and channel area, by stride
/// over the whole `Vertex` ordering. Each carries its whole partition — tens
/// of thousands of branches — and every branch is walked at a fraction of a
/// room's edge, so this is the expensive constant.
const CELL_SAMPLE: usize = 60;

/// How finely a line is walked, as a fraction of a walk-depth room's edge.
/// Task 3 used the same eighth, which is what makes its 1.237% and this
/// file's trunk arm the same measurement.
const SAMPLES_PER_ROOM: f64 = 8.0;

/// The largest coarse basin the Horton sweep will enumerate, in cells. Each
/// carries 25,190 branches under the drawn cut and 16,382 under the even one
/// (derived and measured in
/// [`hornvale_terrain::RILLS_PER_CELL_MAX`]), so this caps the node count
/// rather than saying anything about basins: 40 cells is at most
/// `40 · 32,768` nodes, and the sweep's two parallel `Vec`s are 16 bytes a
/// node, so the arm costs under 21 MB whatever the world does.
const MAX_BASIN_CELLS: usize = 40;

/// Angular separation of two unit vectors, radians.
fn arc(a: [f64; 3], b: [f64; 3]) -> f64 {
    let d = a[0] * b[0] + a[1] * b[1] + a[2] * b[2];
    math::acos(d.clamp(-1.0, 1.0))
}

/// Walk a great-circle segment at `step` radians and insert every walk-depth
/// room the samples land in. Both endpoints are always sampled, so a segment
/// shorter than a step still marks the rooms it starts and ends in.
fn mark(a: [f64; 3], b: [f64; 3], step: f64, rooms: &mut BTreeSet<u64>) {
    let mut curve = BTreeMap::new();
    mark_at(a, b, step, 0, &mut curve);
    rooms.extend(curve.into_keys());
}

/// As [`mark`], but recording the SHALLOWEST octave of the partition that
/// reaches each room — trunks being octave 0 and a branch `d` bisections down
/// being octave `d + 1`.
///
/// The curve this builds is what makes the containment figure honest: a
/// space-filling network's containment is bounded by the resolution it is
/// drawn to, so the headline number on its own is a statement about
/// `RILL_MIN_CATCHMENT` as much as about the world. Reading containment
/// against octave shows which it is — and the run says it is mostly the
/// former: **89.40-89.81%** across the three seeds, of which octave 0 (the
/// trunks) supplies 0.99-1.15% and the remaining 88% arrives steadily from
/// octaves 2 through 16, no single one of them contributing more than about
/// 13 points.
///
/// The figure in this doc used to be a bare `"89%"` with no run behind it. It
/// happens to have been right, which is the least useful way for an unmeasured
/// number to be wrong: it is stated here with the run's own range because a
/// figure nobody measured is a figure nobody can tell has gone stale.
fn mark_at(a: [f64; 3], b: [f64; 3], step: f64, octave: u32, rooms: &mut BTreeMap<u64, u32>) {
    let length = arc(a, b);
    let steps = (length / step).ceil().max(1.0) as usize;
    for i in 0..=steps {
        let t = i as f64 / steps as f64;
        let p = [
            a[0] + t * (b[0] - a[0]),
            a[1] + t * (b[1] - a[1]),
            a[2] + t * (b[2] - a[2]),
        ];
        let n = (p[0] * p[0] + p[1] * p[1] + p[2] * p[2]).sqrt();
        let unit = [p[0] / n, p[1] / n, p[2] / n];
        let id = Facet::containing(unit, WALK_DEPTH)
            .pack()
            .expect("walk depth is inside the address range")
            .0;
        rooms
            .entry(id)
            .and_modify(|o| *o = (*o).min(octave))
            .or_insert(octave);
    }
}

/// The land cells of one seed, sampled by stride over the whole ordering —
/// **land, not reaches**, so a cell the coarse graph gives no outflow counts
/// in the denominator with no channel of its own. Restricting to reaches would
/// measure containment over the land that already has a channel, which is the
/// shape of error the previous Tier 2's 74% was.
fn sampled_land(globe: &TectonicGlobe, geo: &Geosphere, sample: usize) -> Vec<Vertex> {
    // Strided over the LAND cells rather than over all cells, so the sample
    // size is the sample size. Striding over the whole ordering and filtering
    // afterwards leaves a count that varies with the world's land fraction,
    // which is how the first run of this probe measured seed 42 on nineteen
    // cells and reported a trunk arm 34% below Task 3's.
    let land: Vec<Vertex> = geo
        .vertices()
        .filter(|&c| !matches!(*globe.water_kind.get(c), WaterKind::Ocean))
        .collect();
    let stride = (land.len() / sample).max(1);
    land.into_iter().step_by(stride).collect()
}

/// The cell's own stretch of trunk, as up to two great-circle segments —
/// exactly the stretch `branch.rs` hangs the cell's partition from, rebuilt
/// here from the published polyline.
fn trunk_segments(cell: Vertex, net: &ChannelNetwork) -> Vec<[[f64; 3]; 2]> {
    let Some((line, j)) = net.trunk_vertex(cell) else {
        return Vec::new();
    };
    let points = &net.polylines[line].points;
    let here = points[j];
    let mut out = Vec::new();
    if j + 2 == points.len() {
        // THE FAR HALF OF A RUN'S FINAL SEGMENT BELONGS TO NOBODY ELSE. A
        // run's last vertex is an outlet (no trunk vertex of its own) or a
        // confluence cell whose own stretch is on the TRUNK's line, not this
        // one, so this half is never attributed anywhere. Dropping it lost 17%
        // of the rendered trunk length — one half-segment per run — and the
        // first version of this probe duly reported the Tier 1 arm 20% below
        // Task 3's 1.237% for a reason that had nothing to do with the world.
        out.push([here, points[j + 1]]);
    } else {
        out.push([here, midpoint(here, points[j + 1])]);
    }
    if j > 0 {
        out.push([midpoint(points[j - 1], here), here]);
    }
    out
}

/// The point halfway along the great circle between two unit vectors.
fn midpoint(a: [f64; 3], b: [f64; 3]) -> [f64; 3] {
    let s = [a[0] + b[0], a[1] + b[1], a[2] + b[2]];
    let n = (s[0] * s[0] + s[1] * s[1] + s[2] * s[2]).sqrt();
    [s[0] / n, s[1] / n, s[2] / n]
}

/// claim: readout(containment and channel area after Tier 2, against Task 3's
/// 1.237% and 0.2143%, over three seeds)
///
/// **The Tier 1 arm is measured by the same instrument, on the same
/// population, in the same run.** That is the point of it: a Tier 2 figure
/// compared against a number from another task's probe is two instruments and
/// one conclusion, and the trunk arm here should reproduce Task 3's 1.237%
/// independently. Where it does not, the difference is the instrument's and
/// both figures move together.
#[test]
#[ignore = "probe: walks every branch of 60 coarse cells per seed at an eighth of a room (21.8 s measured); run by hand"]
fn containment_and_channel_area_of_the_branch_network() {
    let geo = Geosphere::new(LEVEL);
    let unit = cell_catchment(&geo);
    // Rooms are FACES (`20·4^depth`) and cells are their DUAL
    // (`10·4^level + 2`), so this ratio is not a power of four and mixing the
    // two counts is the factor-of-two error this campaign has now made three
    // times. Stated as one division so there is nowhere for it to hide.
    let rooms_per_cell = (20u64 << (2 * WALK_DEPTH)) as f64 / geo.vertex_count() as f64;
    let room_area = 4.0 * std::f64::consts::PI / (20u64 << (2 * WALK_DEPTH)) as f64;
    let step = room_spacing(&Facet {
        face: 0,
        path: vec![0; WALK_DEPTH as usize],
    }) / SAMPLES_PER_ROOM;
    println!(
        "instrument: {rooms_per_cell:.1} walk-depth rooms per coarse cell, room area \
         {room_area:.4e} sr, sampling every {step:.4e} rad"
    );
    for seed in SEEDS {
        let outcome = generate(Seed(seed), &geo, &TerrainPins::default()).expect("seed generates");
        let globe = &outcome.globe;
        let net = ChannelNetwork::build(globe, &geo, globe.channel_noise_seed());
        let cut = CatchmentCut::Drawn(globe.rill_partition_seed());

        // ARM A — THE CALIBRATION, over every land cell in the world. The
        // channel-area integral is dominated by the few largest discharges
        // (`w ∝ √Q`), so sixty cells estimate it poorly; the trunk arm is
        // cheap enough to run whole-world, and whole-world is what Task 3's
        // 1.237% and 0.2143% are.
        let mut world_rooms: BTreeSet<u64> = BTreeSet::new();
        let mut world_area = 0.0_f64;
        let mut world_cells = 0usize;
        for cell in geo.vertices() {
            if matches!(*globe.water_kind.get(cell), WaterKind::Ocean) {
                continue;
            }
            world_cells += 1;
            let spacing = cell_spacing(&geo, cell);
            for seg in trunk_segments(cell, &net) {
                mark(seg[0], seg[1], step, &mut world_rooms);
                world_area += 2.0
                    * channel_half_width(*globe.drainage.get(cell), spacing)
                    * arc(seg[0], seg[1]);
            }
        }

        let mut trunk_rooms: BTreeSet<u64> = BTreeSet::new();
        let mut all_rooms: BTreeMap<u64, u32> = BTreeMap::new();
        let mut by_depth: BTreeMap<u32, (usize, f64, f64)> = BTreeMap::new();
        let (mut trunk_area, mut branch_area) = (0.0_f64, 0.0_f64);
        let mut cells = 0usize;
        let mut branches = 0usize;
        let mut trunk_length = 0.0_f64;
        let mut branch_length = 0.0_f64;
        let mut spill = 0usize;
        let mut spill_of = 0usize;
        let index = NearestVertexIndex::new(&geo);

        for cell in sampled_land(globe, &geo, CELL_SAMPLE) {
            cells += 1;
            let spacing = cell_spacing(&geo, cell);
            for seg in trunk_segments(cell, &net) {
                mark(seg[0], seg[1], step, &mut trunk_rooms);
                mark_at(seg[0], seg[1], step, 0, &mut all_rooms);
                let half = channel_half_width(*globe.drainage.get(cell), spacing);
                let length = arc(seg[0], seg[1]);
                trunk_area += 2.0 * half * length;
                trunk_length += length;
            }
            for rill in rills_of(cell, &net, &geo, &cut) {
                branches += 1;
                mark_at(rill.head, rill.mouth, step, rill.depth + 1, &mut all_rooms);
                let half = channel_half_width(rill.catchment / unit, spacing);
                let length = arc(rill.head, rill.mouth);
                branch_area += 2.0 * half * length;
                branch_length += length;
                let octave = by_depth.entry(rill.depth).or_insert((0, 0.0, 0.0));
                octave.0 += 1;
                octave.1 += length;
                octave.2 += 2.0 * half * length;
                // THE GEOMETRIC SPILL. A cell's region is a square of its own
                // area, which is a proxy for its real one, so a branch near a
                // corner can land in a neighbour's territory. The scalar is
                // partitioned exactly either way; this measures how much the
                // GEOMETRY overlaps, on every 512th branch so the cost stays
                // linear in the cheap part.
                if branches.is_multiple_of(512) {
                    spill_of += 1;
                    if index.nearest_to_position(&geo, rill.head) != cell {
                        spill += 1;
                    }
                }
            }
        }

        // THE DENOMINATOR AND THE NUMERATOR MUST BE THE SAME POPULATION. The
        // sampled cells are isolated by the stride, so a line may mark a room
        // that belongs to an unsampled neighbour — a cell's region is a square
        // of its own area, which overlaps its neighbours at the corners. Left
        // uncorrected, seed 1234 reported 100.6% containment. So a marked room
        // counts only if the cell nearest its own centroid is one this sweep
        // drew lines for.
        let sampled: BTreeSet<u32> = sampled_land(globe, &geo, CELL_SAMPLE)
            .iter()
            .map(|c| c.0)
            .collect();
        let owns = |id: u64| {
            let addr = hornvale_kernel::FacetId(id)
                .unpack()
                .expect("a packed room unpacks");
            sampled.contains(&index.nearest_to_position(&geo, addr.centroid()).0)
        };
        let trunk_contained = trunk_rooms.iter().filter(|&&id| owns(id)).count();
        let mut curve: BTreeMap<u32, usize> = BTreeMap::new();
        for (&id, &octave) in &all_rooms {
            if owns(id) {
                *curve.entry(octave).or_default() += 1;
            }
        }
        let all_contained: usize = curve.values().sum();
        let denominator = cells as f64 * rooms_per_cell;
        let land_area = cells as f64 * unit;
        let pct = |x: f64| 100.0 * x;
        println!(
            "\nseed {seed}: ARM A, trunks over the whole world: {world_cells} land cells, \
             containment {:.4}%   [Task 3: 1.237%], channel area {:.4}%   [Task 3: 0.2143%]",
            100.0 * world_rooms.len() as f64 / (world_cells as f64 * rooms_per_cell),
            100.0 * world_area / (world_cells as f64 * unit)
        );
        println!(
            "seed {seed}: ARM B, {cells} land cells sampled, {branches} branches, \
             {:.0} walk-depth rooms in the denominator",
            denominator
        );
        println!(
            "  containment, TRUNKS ONLY (the Tier 1 arm, same instrument): {} / {:.0} = \
             {:.4}%   [Task 3, whole world: 1.237%]",
            trunk_contained,
            denominator,
            pct(trunk_contained as f64 / denominator)
        );
        println!(
            "  containment, TRUNKS + BRANCHES: {} / {:.0} = {:.4}%",
            all_contained,
            denominator,
            pct(all_contained as f64 / denominator)
        );
        println!(
            "  channel area, TRUNKS ONLY: {:.4}%   [Task 3, whole world: 0.2143%]",
            pct(trunk_area / land_area)
        );
        println!(
            "  channel area, TRUNKS + BRANCHES: {:.4}%   [R-6's interval: 0.005% - 0.5%]",
            pct((trunk_area + branch_area) / land_area)
        );
        println!(
            "  line length: trunks {trunk_length:.4} rad, branches {branch_length:.4} rad \
             ({:.1}x)",
            branch_length / trunk_length
        );
        println!(
            "  geometric spill: {spill} of {spill_of} sampled branch heads are nearer another \
             cell = {:.2}%",
            pct(spill as f64 / spill_of as f64)
        );
        println!(
            "  R-6 BEST ESTIMATE (whole-world trunk area + sampled branch area): {:.4}%   \
             [interval 0.005% - 0.5%]",
            pct(world_area / (world_cells as f64 * unit) + branch_area / land_area)
        );
        println!(
            "  drainage density: {:.1} rad^-1 of line per unit area (trunks alone {:.1})",
            (trunk_length + branch_length) / land_area,
            trunk_length / land_area
        );
        println!("  containment against resolution — rooms first reached at each octave:");
        let mut cumulative = 0usize;
        for (&octave, &count) in &curve {
            cumulative += count;
            println!(
                "    octave {octave:>2}: +{count:>7} rooms, cumulative {:.4}%",
                pct(cumulative as f64 / denominator)
            );
        }
        if seed == SEEDS[0] {
            // THE OCTAVE TABLE, printed once. Under a `w ∝ √A` width law a
            // branch's wetted area scales as its catchment while the number of
            // branches scales as its reciprocal, so EVERY OCTAVE OF THE
            // PARTITION CONTRIBUTES THE SAME CHANNEL AREA. That is what makes
            // R-6 a bound on how many octaves may be rendered rather than a
            // bound on the coefficient, and it is worth seeing rather than
            // being told.
            println!("  octave   branches      length (rad)     channel area (sr)");
            for (&depth, &(count, length, area)) in &by_depth {
                println!("  {depth:>6}   {count:>8}   {length:>15.5e}   {area:>17.5e}");
            }
        }
    }
}

/// Mean angular separation of a cell from its neighbours — `channel.rs`'s own
/// `cell_spacing`, which is private, restated here from `Geosphere::position`.
fn cell_spacing(geo: &Geosphere, c: Vertex) -> f64 {
    let neighbors = geo.neighbors(c);
    let p = geo.position(c);
    neighbors
        .iter()
        .map(|&n| arc(p, geo.position(n)))
        .sum::<f64>()
        / neighbors.len() as f64
}

/// Strahler order per node over a flow forest given as `outlet[i]`, plus the
/// per-order stream-segment counts and mean segment lengths in radians.
///
/// A **stream segment** of order ω is a maximal chain of nodes all of order ω:
/// it starts where no upstream node shares the order and runs until the order
/// changes. That is the standard Horton-Strahler link, and it is what `R_l`'s
/// "mean length of order ω" means.
fn strahler(outlet: &[Option<u32>], length: &[f64]) -> BTreeMap<u32, (usize, f64)> {
    let n = outlet.len();
    let mut upstream: Vec<Vec<u32>> = vec![Vec::new(); n];
    let mut indegree = vec![0u32; n];
    for (i, &target) in outlet.iter().enumerate() {
        if let Some(t) = target {
            upstream[t as usize].push(i as u32);
            indegree[t as usize] += 1;
        }
    }
    let mut ready: Vec<u32> = (0..n as u32)
        .filter(|&i| indegree[i as usize] == 0)
        .collect();
    let mut order = vec![0u32; n];
    let mut done = 0usize;
    while let Some(i) = ready.pop() {
        done += 1;
        let ups = &upstream[i as usize];
        order[i as usize] = if ups.is_empty() {
            1
        } else {
            let top = ups.iter().map(|&u| order[u as usize]).max().unwrap_or(1);
            let ties = ups.iter().filter(|&&u| order[u as usize] == top).count();
            if ties >= 2 { top + 1 } else { top }
        };
        if let Some(t) = outlet[i as usize] {
            indegree[t as usize] -= 1;
            if indegree[t as usize] == 0 {
                ready.push(t);
            }
        }
    }
    assert_eq!(done, n, "the branch network is not acyclic");

    let mut totals: BTreeMap<u32, (usize, f64)> = BTreeMap::new();
    for i in 0..n {
        let here = order[i];
        if upstream[i].iter().any(|&u| order[u as usize] == here) {
            continue;
        }
        let mut span = 0.0;
        let mut current = i;
        loop {
            span += length[current];
            match outlet[current] {
                Some(t) if order[t as usize] == here => current = t as usize,
                _ => break,
            }
        }
        let entry = totals.entry(here).or_insert((0, 0.0));
        entry.0 += 1;
        entry.1 += span;
    }
    totals
        .into_iter()
        .map(|(w, (count, span))| (w, (count, span / count as f64)))
        .collect()
}

/// The **geometric** mean of the consecutive ratios. Horton's laws are
/// geometric — each order is a constant multiple of the next — so the
/// geometric mean is their estimator and the arithmetic mean is not. The
/// arithmetic mean was the sole cause of the previous Tier 2's only reported
/// miss: one 13-to-1 ratio at the top of a tree dragged the mean of eight
/// terms from 4.0 to 5.05.
///
/// **Read that recommendation narrowly: it is true of Horton's laws and false
/// of this estimator.** Over *consecutive* order ratios the geometric mean
/// **telescopes** — the product of `S_w / S_{w+1}` collapses to
/// `S_1 / S_max`, so this returns
/// `(leaves / top-order segments)^(1/(k−1))` and **sees nothing but the two
/// endpoints**. Every interior order cancels, so a tree can be badly
/// non-Hortonian in the middle and score the same. The classical estimator is
/// a log-regression over *all* orders, which is not this.
///
/// Nothing here is wrong to run — the probe is hand-run and its numbers are
/// real — but do not quote the ratio as a Horton bifurcation ratio without
/// reading The Rill's follow-up 8 in `docs/retrospectives/the-rill.md`, which
/// records why the pass/fail is *likely* robust and why that has **not** been
/// measured.
fn geometric_mean(ratios: &[f64]) -> f64 {
    if ratios.is_empty() {
        return f64::NAN;
    }
    let sum: f64 = ratios.iter().map(|r| math::ln(*r)).sum();
    math::exp(sum / ratios.len() as f64)
}

/// The composite network of one basin — every trunk reach in it, plus every
/// branch of every one of those reaches — as `(outlet, length)` arrays ready
/// for [`strahler`].
fn basin_network(
    basin: &[Vertex],
    globe: &TectonicGlobe,
    geo: &Geosphere,
    net: &ChannelNetwork,
    cut: Option<&CatchmentCut>,
) -> (Vec<Option<u32>>, Vec<f64>) {
    // Trunk nodes first, one per basin cell, so their indices are the slot
    // every branch tree hangs off.
    let slot: BTreeMap<u32, u32> = basin
        .iter()
        .enumerate()
        .map(|(i, c)| (c.0, i as u32))
        .collect();
    let mut outlet: Vec<Option<u32>> = Vec::with_capacity(basin.len());
    let mut length: Vec<f64> = Vec::with_capacity(basin.len());
    for &cell in basin {
        outlet.push(
            globe
                .downhill
                .get(cell)
                .and_then(|t| slot.get(&t.0).copied()),
        );
        length.push(
            trunk_segments(cell, net)
                .iter()
                .map(|seg| arc(seg[0], seg[1]))
                .sum(),
        );
    }
    // Then every branch of every basin cell, unless the caller asked for the
    // trunk network alone — which is the arm that isolates the WORLD's own
    // ratios from the partition's.
    let Some(cut) = cut else {
        return (outlet, length);
    };
    for (i, &cell) in basin.iter().enumerate() {
        let rills: Vec<Rill> = rills_of(cell, net, geo, cut);
        let base = outlet.len() as u32;
        for rill in &rills {
            outlet.push(Some(match rill.parent {
                Some(p) => base + p as u32,
                None => i as u32,
            }));
            length.push(arc(rill.head, rill.mouth));
        }
    }
    (outlet, length)
}

/// claim: readout(R-5 — Horton's bifurcation and length ratios of the Tier 2
/// network at branch resolution, over three seeds, WITH the falsification arm)
///
/// **The composite tree, not the motif**: a whole coarse basin's trunks
/// together with every branch hanging off them, so the terrain-driven trunk
/// network and the drawn partition below it are both in the population. A
/// basin is self-contained by construction — its only exit is the cell its
/// coarse accumulation stops at — so no boundary inflow is missing.
///
/// **The falsification runs in the same sweep and is printed beside the
/// result.** Under [`CatchmentCut::Even`] the drawn freedom is held constant
/// and the partition is balanced; if the ratios do not move between the two
/// arms then they are the rule's and not the partition's, and R-5 is
/// untestable as posed. That is what happened to the quadrisecting design this
/// replaces, and it was only visible after the fact.
#[test]
#[ignore = "probe: enumerates every branch of a whole coarse basin, twice (1.4 s measured); run by hand"]
fn horton_ratios_of_the_branch_network() {
    let geo = Geosphere::new(LEVEL);
    for seed in SEEDS {
        let outcome = generate(Seed(seed), &geo, &TerrainPins::default()).expect("seed generates");
        let globe = &outcome.globe;
        let net = ChannelNetwork::build(globe, &geo, globe.channel_noise_seed());

        // Basins: group reaches by the cell their downhill chain terminates in.
        let is_reach = |c: Vertex| {
            !matches!(*globe.water_kind.get(c), WaterKind::Ocean) && globe.downhill.get(c).is_some()
        };
        let mut basins: BTreeMap<u32, Vec<Vertex>> = BTreeMap::new();
        for c in geo.vertices() {
            if !is_reach(c) {
                continue;
            }
            let mut current = c;
            let mut hops = 0usize;
            let terminal = loop {
                match *globe.downhill.get(current) {
                    None => break current,
                    Some(next) => {
                        current = next;
                        hops += 1;
                        assert!(hops < geo.vertex_count(), "the coarse graph cycles");
                    }
                }
            };
            basins.entry(terminal.0).or_default().push(c);
        }
        let basin = basins
            .values()
            .filter(|members| members.len() <= MAX_BASIN_CELLS)
            .max_by_key(|members| members.len())
            .expect("some basin fits the cap")
            .clone();
        assert!(basin.len() > 10, "seed {seed}: no basin worth measuring");

        // The largest basin in the world, for the trunk-only arm: no branches
        // means no node-count problem, and the trunk network's own ratios are
        // the only thing in this measurement the WORLD decides.
        let widest = basins
            .values()
            .max_by_key(|members| members.len())
            .expect("some basin exists")
            .clone();
        let drawn = CatchmentCut::Drawn(globe.rill_partition_seed());
        for (name, cells, cut) in [
            ("TRUNKS  ", &widest, None),
            ("DRAWN   ", &basin, Some(&drawn)),
            ("EVEN    ", &basin, Some(&CatchmentCut::Even)),
        ] {
            let (outlet, length) = basin_network(cells, globe, &geo, &net, cut);
            let summary = strahler(&outlet, &length);
            let mut bifurcation = Vec::new();
            let mut ratios = Vec::new();
            for (&w, &(count, mean)) in &summary {
                if let Some(&(next_count, next_mean)) = summary.get(&(w + 1)) {
                    bifurcation.push(count as f64 / next_count as f64);
                    ratios.push(next_mean / mean);
                }
            }
            println!(
                "\nseed {seed} [{name}]: basin of {} cells, {} nodes, highest order {}",
                cells.len(),
                outlet.len(),
                summary.keys().next_back().copied().unwrap_or(0)
            );
            println!("  order   segments   mean length (rad)");
            for (&w, &(count, mean)) in &summary {
                println!("  {w:>5}   {count:>8}   {mean:>17.6e}");
            }
            println!(
                "  R_b = {:.6} (geometric)   per-order {:?}",
                geometric_mean(&bifurcation),
                bifurcation
                    .iter()
                    .map(|r| (r * 1000.0).round() / 1000.0)
                    .collect::<Vec<_>>()
            );
            println!(
                "  R_l = {:.6} (geometric)   per-order {:?}",
                geometric_mean(&ratios),
                ratios
                    .iter()
                    .map(|r| (r * 1000.0).round() / 1000.0)
                    .collect::<Vec<_>>()
            );
        }
    }
}
