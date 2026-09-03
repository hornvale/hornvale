//! The Plat, Task 0 — print the support of the plan attributes the
//! inhabited reading will be derived from, BEFORE the derivation is designed
//! against them (the Brattice's retrospective: "print the support of any
//! exported value before designing against it").
//!
//! Per level of every plan in the sample, this prints the distribution of:
//!
//! - **degree** — the maximum node degree, and how many nodes reach 3. The
//!   heart (Alexander 129) is a hub; a level whose every node has degree ≤ 2
//!   is a path or a ring and has no heart to name.
//! - **the heart** — the graph median under the plan's own metric (paths
//!   through stairs, summed over the level's nodes, the arrival excluded),
//!   and how many within-level components a level has.
//! - **degeneracy** — whether the hub is the level's arrival node (the
//!   shallowest by `depth`) or its sanctum (the deepest), which would make
//!   "heart" a second name for a node that already has one.
//! - **the sanctum and the keys** — how often the deepest node on a level is
//!   also a node the Brattice placed a key on, which decides whether a hoarder
//!   sitting at the sanctum sits on a key by construction or by chance.
//! - **the landing** — a node that is the lower end of one stair and the
//!   upper end of another (Alexander 133's stage), which the Brattice removed
//!   from its inventory because the realm class it wanted never occurs; here
//!   the question is asked of nodes rather than realms.
//! - **circulation realms** (Alexander 98) — whether a nested realm's region
//!   area is smaller than its parent's.
//!
//! A probe, not a test: it asserts nothing and is run by hand. Vertex 2, the
//! Brattice's own sample vertex, so the two campaigns' numbers are comparable.

use std::collections::BTreeMap;

use hornvale_kernel::{Band, Seed, Vertex};
use hornvale_terrain::CaveKind;
use hornvale_worldgen::character::Character;
use hornvale_worldgen::circuit::{DescentPlan, NodeId, plan_descent};

fn habitation_rungs() -> Vec<Band> {
    hornvale_terrain::rungs()
        .iter()
        .copied()
        .filter(|r| *r != Band::Surface)
        .collect()
}

#[derive(Default)]
struct Tally {
    levels: usize,
    nodes_per_level: BTreeMap<usize, usize>,
    max_degree: BTreeMap<usize, usize>,
    hubs_per_level: BTreeMap<usize, usize>,
    hub_is_arrival: usize,
    median_is_arrival: usize,
    median_is_sanctum: usize,
    median_is_hub: usize,
    median_degree: BTreeMap<usize, usize>,
    median_position: BTreeMap<u16, usize>,
    median_ties: BTreeMap<usize, usize>,
    components: BTreeMap<usize, usize>,
    hub_is_sanctum: usize,
    arrival_is_sanctum: usize,
    sanctum_has_key: usize,
    levels_with_key: usize,
    key_at_sanctum_given_key: usize,
    landings: usize,
    levels_with_landing: usize,
    depth_range: BTreeMap<u16, usize>,
    nested_realms: usize,
    nested_smaller: usize,
}

fn area(plan: &DescentPlan, node: NodeId) -> i64 {
    let r = plan.region_of(node);
    r.w as i64 * r.h as i64
}

fn realm_area(plan: &DescentPlan, realm: usize) -> i64 {
    let r = &plan.realms[realm];
    let mut nodes: Vec<NodeId> = r.path_a.iter().chain(r.path_b.iter()).copied().collect();
    nodes.sort_unstable();
    nodes.dedup();
    nodes.iter().map(|&n| area(plan, n)).sum()
}

fn tally(plan: &DescentPlan, t: &mut Tally) {
    let keys = hornvale_worldgen::brattice::key_nodes(plan);
    for level in 0..plan.rungs.len() {
        let nodes = plan.nodes_on(level);
        if nodes.is_empty() {
            continue;
        }
        t.levels += 1;
        *t.nodes_per_level.entry(nodes.len()).or_default() += 1;
        let degree = |n: NodeId| plan.neighbours(n).len();
        let depth = |n: NodeId| plan.nodes[n].depth;
        let max_deg = nodes.iter().map(|&n| degree(n)).max().unwrap_or(0);
        *t.max_degree.entry(max_deg).or_default() += 1;
        let hubs = nodes.iter().filter(|&&n| degree(n) >= 3).count();
        *t.hubs_per_level.entry(hubs).or_default() += 1;
        // Ties: lower depth, then lower id — the same tie-break the reading
        // will state, so the probe measures the rule it is measuring for.
        let hub = nodes
            .iter()
            .copied()
            .max_by(|&a, &b| {
                degree(a)
                    .cmp(&degree(b))
                    .then_with(|| depth(b).cmp(&depth(a)))
                    .then_with(|| b.cmp(&a))
            })
            .expect("non-empty");
        let arrival = nodes
            .iter()
            .copied()
            .min_by(|&a, &b| depth(a).cmp(&depth(b)).then_with(|| a.cmp(&b)))
            .expect("non-empty");
        let sanctum = nodes
            .iter()
            .copied()
            .max_by(|&a, &b| depth(a).cmp(&depth(b)).then_with(|| b.cmp(&a)))
            .expect("non-empty");
        // Alexander 129's own words: the common area sits at the CENTER OF
        // GRAVITY of the spaces the group occupies. On a graph that is the
        // median — the node minimizing the sum of hop distances to every
        // other node on the level. Ties: higher degree, lower depth, lower id.
        // Through the WHOLE plan, stairs included: a level's within-level
        // passage graph can be disconnected (its islands joined only through
        // the level above), and a within-level sum would let a small island
        // win trivially. Summed over this level's nodes only.
        let dist_sum = |n: NodeId| -> usize {
            let mut seen: BTreeMap<NodeId, usize> = BTreeMap::new();
            let mut frontier = std::collections::VecDeque::from([n]);
            seen.insert(n, 0);
            while let Some(cur) = frontier.pop_front() {
                let d = seen[&cur];
                for m in plan.neighbours(cur) {
                    if seen.contains_key(&m) {
                        continue;
                    }
                    seen.insert(m, d + 1);
                    frontier.push_back(m);
                }
            }
            nodes.iter().map(|m| seen[m]).sum()
        };
        // Within-level components, for the report.
        let components = {
            let mut unseen: std::collections::BTreeSet<NodeId> = nodes.iter().copied().collect();
            let mut count = 0usize;
            while let Some(&start) = unseen.iter().next() {
                count += 1;
                let mut stack = vec![start];
                unseen.remove(&start);
                while let Some(cur) = stack.pop() {
                    for m in plan.neighbours(cur) {
                        if plan.nodes[m].level as usize == level && unseen.remove(&m) {
                            stack.push(m);
                        }
                    }
                }
            }
            count
        };
        *t.components.entry(components).or_default() += 1;
        let sums: BTreeMap<NodeId, usize> = nodes.iter().map(|&n| (n, dist_sum(n))).collect();
        // The Entry is excluded from the heart's candidates by rule (Alexander
        // 112: there is always a transition between the outside and the heart).
        let best = sums
            .iter()
            .filter(|(n, _)| **n != arrival)
            .map(|(_, v)| *v)
            .min()
            .unwrap_or(0);
        let ties = sums
            .iter()
            .filter(|(n, v)| **n != arrival && **v == best)
            .count();
        *t.median_ties.entry(ties).or_default() += 1;
        let median = nodes
            .iter()
            .copied()
            .filter(|n| *n != arrival && sums[n] == best)
            .max_by(|&a, &b| {
                degree(a)
                    .cmp(&degree(b))
                    .then_with(|| depth(b).cmp(&depth(a)))
                    .then_with(|| b.cmp(&a))
            })
            .unwrap_or(arrival);
        if median == arrival {
            t.median_is_arrival += 1;
        }
        if median == sanctum {
            t.median_is_sanctum += 1;
        }
        if median == hub {
            t.median_is_hub += 1;
        }
        *t.median_degree.entry(degree(median)).or_default() += 1;
        let range = depth(sanctum) - depth(arrival);
        let pos = if range == 0 {
            0
        } else {
            ((depth(median) - depth(arrival)) as u32 * 10 / range as u32) as u16
        };
        *t.median_position.entry(pos).or_default() += 1;
        if max_deg >= 3 {
            if hub == arrival {
                t.hub_is_arrival += 1;
            }
            if hub == sanctum {
                t.hub_is_sanctum += 1;
            }
        }
        if arrival == sanctum {
            t.arrival_is_sanctum += 1;
        }
        let level_has_key = nodes.iter().any(|n| keys.contains(n));
        if level_has_key {
            t.levels_with_key += 1;
            if keys.contains(&sanctum) {
                t.key_at_sanctum_given_key += 1;
            }
        }
        if keys.contains(&sanctum) {
            t.sanctum_has_key += 1;
        }
        let uppers: Vec<NodeId> = plan.stairs_from(level).iter().map(|s| s.0).collect();
        let lowers: Vec<NodeId> = plan.stairs_into(level).iter().map(|s| s.1).collect();
        let landings = nodes
            .iter()
            .filter(|n| uppers.contains(n) && lowers.contains(n))
            .count();
        t.landings += landings;
        if landings > 0 {
            t.levels_with_landing += 1;
        }
        let range = depth(sanctum) - depth(arrival);
        *t.depth_range.entry(range).or_default() += 1;
    }
    for (i, r) in plan.realms.iter().enumerate() {
        if let Some(p) = r.parent {
            t.nested_realms += 1;
            if realm_area(plan, i) < realm_area(plan, p) {
                t.nested_smaller += 1;
            }
        }
    }
}

fn pct(n: usize, d: usize) -> String {
    if d == 0 {
        "n/a".to_string()
    } else {
        format!("{:.1}%", 100.0 * n as f64 / d as f64)
    }
}

/// claim: rate(seed: 0..200) — a distribution printed for design, never an
/// assertion; the numbers are the support of `depth`, degree and stairs over
/// the sample, not a property held to a value.
#[test]
#[ignore = "probe: the support of the plan attributes the inhabited reading derives from (The Plat, Task 0); run by hand"]
fn support_of_the_attributes_the_inhabited_reading_derives_from() {
    const SEEDS: u64 = 200;
    let rungs = habitation_rungs();
    for character in [Character::DrowTier, Character::WildCave] {
        for kind in [CaveKind::Karst, CaveKind::Fracture, CaveKind::LavaTube] {
            let mut t = Tally::default();
            for s in 0..SEEDS {
                let plan = plan_descent(Seed(s), Vertex(2), &rungs, kind, character);
                tally(&plan, &mut t);
            }
            println!(
                "== {character:?} × {kind:?} — {} levels over {SEEDS} plans ==",
                t.levels
            );
            println!("  nodes per level      {:?}", t.nodes_per_level);
            println!("  max degree           {:?}", t.max_degree);
            println!("  nodes of degree >= 3 {:?}", t.hubs_per_level);
            let with_hub: usize = t
                .max_degree
                .iter()
                .filter(|(d, _)| **d >= 3)
                .map(|(_, n)| n)
                .sum();
            println!(
                "  levels with a hub    {} ({})  hub==arrival {}  hub==sanctum {}",
                with_hub,
                pct(with_hub, t.levels),
                pct(t.hub_is_arrival, with_hub),
                pct(t.hub_is_sanctum, with_hub)
            );
            println!(
                "  median (heart)       ==arrival {}  ==sanctum {}  ==max-degree hub {}  degree {:?}  ties {:?}",
                pct(t.median_is_arrival, t.levels),
                pct(t.median_is_sanctum, t.levels),
                pct(t.median_is_hub, t.levels),
                t.median_degree,
                t.median_ties
            );
            println!("  components per lvl   {:?}", t.components);
            println!(
                "  median depth decile  {:?}  (0 = arrival, 10 = sanctum)",
                t.median_position
            );
            println!(
                "  arrival==sanctum     {} ({})",
                t.arrival_is_sanctum,
                pct(t.arrival_is_sanctum, t.levels)
            );
            println!("  depth range          {:?}", t.depth_range);
            println!(
                "  levels with a key    {} ({}); key AT the sanctum given a key {}; sanctum has key over all levels {}",
                t.levels_with_key,
                pct(t.levels_with_key, t.levels),
                pct(t.key_at_sanctum_given_key, t.levels_with_key),
                pct(t.sanctum_has_key, t.levels)
            );
            println!(
                "  landings             {} nodes on {} levels ({})",
                t.landings,
                t.levels_with_landing,
                pct(t.levels_with_landing, t.levels)
            );
            println!(
                "  nested realms        {}; smaller than parent {} ({})",
                t.nested_realms,
                t.nested_smaller,
                pct(t.nested_smaller, t.nested_realms)
            );
        }
    }
}
