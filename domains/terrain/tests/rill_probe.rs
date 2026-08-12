//! The Rill, Task 4: the Tier 2 measurement instrument — Horton's laws (R-5)
//! and the containment the subdivision recovers.
//!
//! Both are **measurements, not gates**, so both are `#[ignore]`d under the
//! `probe:` token `rift_probe.rs` established: they enumerate millions of
//! walk-depth rooms and take minutes. Run them by hand:
//!
//! ```text
//! cargo test -p hornvale-terrain --test rill_probe -- --ignored --nocapture
//! ```
//!
//! Nothing here asserts a preregistered range. R-5's `[3.0, 5.0]` and
//! `[1.5, 3.5]` are the empirically observed ranges for **real river
//! networks** — a reference entirely outside this codebase — and the campaign's
//! instruction is to take the branch and report, not to tune the generator
//! until it lands inside them. The verdict is scored in the task report from
//! the numbers this file prints.

use hornvale_kernel::{CellId, Geosphere, NearestCellIndex, RoomAddr, Seed};
use hornvale_terrain::{
    TerrainPins, channel_half_width, floor_flow, flow_at, generate, room_spacing,
};
use std::collections::BTreeMap;

/// The seeds every measurement here sweeps — the campaign's usual trio.
const SEEDS: [u64; 3] = [42, 7, 1234];

/// The canonical grid the coarse flow graph lives on.
const LEVEL: u32 = 6;

/// Walk depth: the room depth `windows/locale` places a walker at, and the
/// depth every number in this file is stated at.
const WALK_DEPTH: u32 = 12;

/// The largest coarse basin the Horton sweep will enumerate, in globe-level
/// faces. Each face carries `4⁶ = 4096` walk-depth rooms, so this is a cap on
/// the room count (and therefore on the runtime and the memory) rather than a
/// claim about basins.
const MAX_BASIN_FACES: usize = 300;

/// The room at `index` in the depth-`depth` face ordering — `Geosphere`'s own
/// subdivision order, in which child `k` of face `i` is face `4i + k`.
fn room_of(index: u64, depth: u32) -> RoomAddr {
    let span = 1u64 << (2 * depth);
    let mut path = Vec::with_capacity(depth as usize);
    for step in (0..depth).rev() {
        path.push(((index % span) >> (2 * step) & 0b11) as u8);
    }
    RoomAddr {
        face: (index / span) as u8,
        path,
    }
}

/// The inverse of [`room_of`].
fn index_of(addr: &RoomAddr) -> u64 {
    addr.path
        .iter()
        .fold(u64::from(addr.face), |acc, &d| acc * 4 + u64::from(d))
}

/// Strahler order per room over a flow forest given as `outlet[i]`, plus the
/// per-order stream-segment counts and mean segment lengths in rooms.
///
/// A **stream segment** of order ω is a maximal chain of rooms all of order ω:
/// it starts where no upstream room shares the order (a source, or the room
/// just below a junction that raised the order) and runs until the order
/// changes. That is the standard Horton–Strahler link, and it is what `R_l`'s
/// "mean length of order ω" means.
fn strahler(outlet: &[Option<u32>]) -> (Vec<u32>, BTreeMap<u32, (usize, f64)>) {
    let n = outlet.len();
    let mut upstream: Vec<Vec<u32>> = vec![Vec::new(); n];
    let mut indegree = vec![0u32; n];
    for (i, &target) in outlet.iter().enumerate() {
        if let Some(t) = target {
            upstream[t as usize].push(i as u32);
            indegree[t as usize] += 1;
        }
    }
    // Kahn: the flow graph is acyclic (the floor lift is strictly monotone in
    // coarse drainage and the subdivision drains inward), so every room is
    // reached exactly once.
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
    assert_eq!(done, n, "the sub-cell flow graph is not acyclic");

    // Segment starts: a room whose order no upstream room shares.
    let mut totals: BTreeMap<u32, (usize, usize)> = BTreeMap::new();
    for i in 0..n {
        let here = order[i];
        if upstream[i].iter().any(|&u| order[u as usize] == here) {
            continue;
        }
        let mut length = 0usize;
        let mut current = i;
        loop {
            length += 1;
            match outlet[current] {
                Some(t) if order[t as usize] == here => current = t as usize,
                _ => break,
            }
        }
        let entry = totals.entry(here).or_insert((0, 0));
        entry.0 += 1;
        entry.1 += length;
    }
    let summary = totals
        .into_iter()
        .map(|(w, (count, rooms))| (w, (count, rooms as f64 / count as f64)))
        .collect();
    (order, summary)
}

/// The mean of the consecutive ratios `f(ω)/f(ω+1)` (bifurcation) or
/// `f(ω+1)/f(ω)` (length), over the orders that have both terms. Horton's laws
/// are geometric, so the mean of the consecutive ratios is the estimator; the
/// per-order table is printed alongside so a reader can see whether it is one.
fn ratio_mean(pairs: &[f64]) -> f64 {
    if pairs.is_empty() {
        return f64::NAN;
    }
    pairs.iter().sum::<f64>() / pairs.len() as f64
}

/// claim: readout(R-5 — Horton's bifurcation and length ratios of the Tier
/// 2 sub-cell network at walk depth, over three seeds)
///
/// **The composite tree, not the motif.** The network measured here is the real
/// one: a whole coarse basin's worth of walk-depth rooms, so the terrain-driven
/// floor lift and the six levels of subdivision below it are both in it. A
/// basin is self-contained by construction — its only exit is the terminal face
/// the coarse accumulation stops at — so the tree is complete and no boundary
/// inflow is missing from it.
#[test]
#[ignore = "probe: enumerates a whole basin of walk-depth rooms (minutes); run by hand"]
fn horton_ratios_of_the_sub_cell_network() {
    let geo = Geosphere::new(LEVEL);
    let index = NearestCellIndex::new(&geo);
    let faces = 20u64 << (2 * LEVEL);
    for seed in SEEDS {
        let outcome = generate(Seed(seed), &geo, &TerrainPins::default()).expect("seed generates");
        let globe = &outcome.globe;
        let sub_seed = globe.subcell_flow_seed();

        // The coarse face graph: which face each flowing face drains into.
        let mut face_outlet: Vec<Option<u64>> = vec![None; faces as usize];
        let mut flowing = 0usize;
        for f in 0..faces {
            let room = room_of(f, LEVEL);
            if let Some((_, outlet)) = floor_flow(&room, globe, &geo, &index) {
                face_outlet[f as usize] = Some(index_of(&outlet));
                flowing += 1;
            }
        }
        // Basins: group flowing faces by the terminal face they reach.
        let mut terminal_of: Vec<Option<u64>> = vec![None; faces as usize];
        for f in 0..faces {
            if face_outlet[f as usize].is_none() {
                continue;
            }
            let mut current = f;
            let mut hops = 0usize;
            let terminal = loop {
                match face_outlet[current as usize] {
                    None => break current,
                    Some(next) => {
                        current = next;
                        hops += 1;
                        assert!(hops < faces as usize, "the coarse face graph cycles");
                    }
                }
            };
            terminal_of[f as usize] = Some(terminal);
        }
        let mut basins: BTreeMap<u64, Vec<u64>> = BTreeMap::new();
        for f in 0..faces {
            if let Some(t) = terminal_of[f as usize] {
                basins.entry(t).or_default().push(f);
            }
        }
        // The largest basin that fits the room cap — the biggest real network
        // available rather than a convenient corner of one.
        let mut chosen: Vec<u64> = Vec::new();
        for members in basins.values() {
            if members.len() <= MAX_BASIN_FACES && members.len() > chosen.len() {
                chosen = members.clone();
            }
        }
        let basin_faces = chosen.len();
        assert!(basin_faces > 20, "seed {seed}: no basin worth measuring");

        // Every walk-depth room of the basin, densely indexed.
        let per_face = 1u64 << (2 * (WALK_DEPTH - LEVEL));
        let slot: BTreeMap<u64, usize> = chosen.iter().enumerate().map(|(i, &f)| (f, i)).collect();
        let n = basin_faces * per_face as usize;
        let mut outlet: Vec<Option<u32>> = vec![None; n];
        for (i, &f) in chosen.iter().enumerate() {
            for sub in 0..per_face {
                let global = f * per_face + sub;
                let room = room_of(global, WALK_DEPTH);
                let flow = flow_at(&room, globe, &geo, &index, sub_seed)
                    .expect("every room of a flowing face has flow");
                let target = index_of(&flow.outlet);
                let host = target / per_face;
                outlet[i * per_face as usize + sub as usize] = slot
                    .get(&host)
                    .map(|&j| (j * per_face as usize + (target % per_face) as usize) as u32);
            }
        }
        let (order, summary) = strahler(&outlet);
        let highest = order.iter().copied().max().unwrap_or(0);
        println!(
            "\nseed {seed}: basin of {basin_faces} coarse faces = {n} walk-depth rooms, \
             {flowing} flowing faces in the world, highest Strahler order {highest}"
        );
        println!("  order   segments   mean length (rooms)");
        let mut bifurcation = Vec::new();
        let mut length = Vec::new();
        for (&w, &(count, mean)) in &summary {
            println!("  {w:>5}   {count:>8}   {mean:>19.3}");
            if let Some(&(next_count, next_mean)) = summary.get(&(w + 1)) {
                bifurcation.push(count as f64 / next_count as f64);
                length.push(next_mean / mean);
            }
        }
        println!(
            "  R_b = {:.4}   (per-order {:?})",
            ratio_mean(&bifurcation),
            bifurcation
                .iter()
                .map(|r| (r * 1000.0).round() / 1000.0)
                .collect::<Vec<_>>()
        );
        println!(
            "  R_l = {:.4}   (per-order {:?})",
            ratio_mean(&length),
            length
                .iter()
                .map(|r| (r * 1000.0).round() / 1000.0)
                .collect::<Vec<_>>()
        );
    }
}

/// How many walk-depth rooms the containment sweep samples per seed. A stride
/// over the whole `20·4¹² = 335,544,320` room ordering, so the sample is
/// uniform over the sphere rather than over one region.
const CONTAINMENT_SAMPLE: u64 = 400_000;

/// claim: readout(the containment Tier 2 recovers, against Task 3's
/// 1.237%, over three seeds)
///
/// Counts, over walk-depth rooms sampled uniformly on the sphere and restricted
/// to land (the denominator Task 3 used — `20·4¹²` rooms times the land
/// fraction):
///
/// - **containment**: a sub-cell channel passes through the room. Under Tier 2
///   that is exactly "the room has flow", because every room with flow carries
///   its own reach from its centroid to its outlet's.
/// - **resolved**: the room's own channel is at least as wide as the room, so a
///   walker standing anywhere in it is in water. This is the statistic Task 3's
///   centroid census was reaching for, restated at a scale where "the centroid
///   is on the line" is true by construction and therefore says nothing.
/// - **channel area fraction**: the tube integral `Σ 2·half_width·spacing` over
///   land area, comparable to Task 3's row 3.
#[test]
#[ignore = "probe: samples 400k walk-depth rooms per seed (minutes); run by hand"]
fn containment_of_the_sub_cell_network() {
    let geo = Geosphere::new(LEVEL);
    let index = NearestCellIndex::new(&geo);
    let rooms = 20u64 << (2 * WALK_DEPTH);
    let stride = rooms / CONTAINMENT_SAMPLE;
    for seed in SEEDS {
        let outcome = generate(Seed(seed), &geo, &TerrainPins::default()).expect("seed generates");
        let globe = &outcome.globe;
        let sub_seed = globe.subcell_flow_seed();
        let (mut sampled, mut land, mut contained, mut resolved) = (0usize, 0usize, 0usize, 0usize);
        let mut channel_area = 0.0_f64;
        let mut land_area = 0.0_f64;
        let mut widest = 0.0_f64;
        for step in 0..CONTAINMENT_SAMPLE {
            let room = room_of(step * stride, WALK_DEPTH);
            sampled += 1;
            let here: CellId = index.nearest_to_position(&geo, room.centroid());
            if *globe.elevation.get(here) < globe.sea_level {
                continue;
            }
            land += 1;
            let spacing = room_spacing(&room);
            // A room's own solid angle: equilateral spherical triangle of edge
            // `spacing`, taken as the planar area, which is exact to a part in
            // 10^7 at this depth.
            let area = 0.4330127018922193 * spacing * spacing;
            land_area += area;
            let Some(flow) = flow_at(&room, globe, &geo, &index, sub_seed) else {
                continue;
            };
            contained += 1;
            let half = channel_half_width(flow.upstream, spacing);
            widest = widest.max(half);
            if 2.0 * half >= spacing {
                resolved += 1;
            }
            channel_area += (2.0 * half).min(spacing) * spacing;
        }
        let pct = |a: usize, b: usize| 100.0 * a as f64 / b as f64;
        println!(
            "\nseed {seed}: {sampled} walk-depth rooms sampled, {land} on land \
             ({:.3}% land fraction)",
            pct(land, sampled)
        );
        println!(
            "  containment (a sub-cell channel passes through the room): {contained} / {land} \
             = {:.4}%   [Task 3's coarse network: 1.237%]",
            pct(contained, land)
        );
        println!(
            "  resolved (the room's own channel is at least as wide as the room): {resolved} \
             / {land} = {:.4}%",
            pct(resolved, land)
        );
        println!(
            "  channel area fraction (tube integral over land area): {:.4}%   \
             [Task 3's row 3: 0.2143%]",
            100.0 * channel_area / land_area
        );
        println!(
            "  widest sub-cell half-width seen: {widest:.4e} rad, against a walk-depth room \
             spacing of {:.4e} rad",
            room_spacing(&room_of(0, WALK_DEPTH))
        );
    }
}
