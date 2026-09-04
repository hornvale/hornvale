//! The inhabited reading of a descent plan (The Plat; spec §3.1).
//!
//! **Use follows form.** A people does not design the graph — the rock and
//! the draws did — it moves in and reads it: the shallowest room is the
//! door, the room every path passes near is the hall, the deepest is theirs
//! alone. This module is that reading: a pure function over a finished
//! [`DescentPlan`] that adds no draw, consumes no stream and rewrites no
//! edge, so decision 0618's epoch is never engaged and no plan byte moves.
//!
//! Per level: exactly one [`Role::Entry`] (least `depth`), exactly one
//! [`Role::Sanctum`] (greatest `depth`), at most one [`Role::Heart`] — the
//! graph median among the level's nodes with the Entry excluded, under the
//! PLAN's metric (shortest paths through any level, stairs included), because
//! a level's within-level passage graph is usually in pieces (The Plat's
//! Task 0 probe: only 23% of karst levels are one piece) — and
//! [`Role::Chamber`] for the rest. A node is a `landing` when it is the lower
//! end of one stair and the upper end of another (Alexander 133). `rank` is
//! the nesting depth of a node's innermost realm (Alexander 98).
//!
//! The reading is computed for EVERY plan, wild or worked: the hoarder reads
//! it everywhere (`windows/vessel`), and only its vocabulary is keyed to a
//! Made rung. Nothing here reads a seed, a vertex, terrain or a ledger, which
//! is what lets The Precincts lift it over a district graph.

use std::collections::{BTreeMap, VecDeque};

use crate::circuit::{DescentPlan, NodeId};

/// What a node is to the people who live by the plan (spec §3.1).
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Role {
    /// The shallowest node of its level — the door, and the transition
    /// between outside and the heart (Alexander 112, 127's public end).
    Entry,
    /// The center of gravity of the level's rooms: the graph median under the
    /// plan's metric, the Entry excluded (Alexander 129).
    Heart,
    /// The deepest node of its level — the most private domain (Alexander
    /// 127's private end; the hoarder's seat).
    Sanctum,
    /// Every other node; its intimacy is its own `depth`.
    Chamber,
}

/// The reading of one plan, parallel to `plan.nodes`.
/// type-audit: bare-ok(flag: landing), bare-ok(count: rank)
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Reading {
    /// Each node's role on its level.
    pub roles: Vec<Role>,
    /// True where a node is the lower end of one stair and the upper end of
    /// another — the staircase as a stage (Alexander 133).
    pub landing: Vec<bool>,
    /// The nesting depth of the node's innermost realm through
    /// `Realm.parent`; 0 for a spine node on no cycle (Alexander 98).
    pub rank: Vec<u8>,
}

/// Read a finished plan. Total and deterministic; reads nothing but `plan`.
pub fn read(plan: &DescentPlan) -> Reading {
    let n = plan.nodes.len();
    let mut roles = vec![Role::Chamber; n];
    let mut landing = vec![false; n];
    let mut rank = vec![0u8; n];

    for level in 0..plan.rungs.len() {
        let nodes = plan.nodes_on(level);
        if nodes.is_empty() {
            continue;
        }
        let depth = |i: NodeId| plan.nodes[i].depth;
        // Entry: least depth, ties to the lower id.
        let entry = nodes
            .iter()
            .copied()
            .min_by(|&a, &b| depth(a).cmp(&depth(b)).then_with(|| a.cmp(&b)))
            .expect("non-empty level");
        // Sanctum: greatest depth, ties to the lower id.
        let sanctum = nodes
            .iter()
            .copied()
            .max_by(|&a, &b| depth(a).cmp(&depth(b)).then_with(|| b.cmp(&a)))
            .expect("non-empty level");
        roles[entry] = Role::Entry;
        // A one-node level is its own Entry and Sanctum; Entry wins the
        // name because the possession stands there. The probe never saw one
        // (depth range >= 2 on every level), so this is a total-function
        // arm, not a case the world produces.
        if sanctum != entry {
            roles[sanctum] = Role::Sanctum;
        }
        // Heart: the graph median among the rest, under the plan's metric.
        let sums: BTreeMap<NodeId, u32> = nodes
            .iter()
            .filter(|&&i| i != entry)
            .map(|&i| (i, distance_sum(plan, i, &nodes)))
            .collect();
        if let Some(best) = sums.values().copied().min() {
            let degree = |i: NodeId| plan.neighbours(i).len();
            let heart = sums
                .iter()
                .filter(|&(_, &s)| s == best)
                .map(|(&i, _)| i)
                .max_by(|&a, &b| {
                    degree(a)
                        .cmp(&degree(b))
                        .then_with(|| depth(b).cmp(&depth(a)))
                        .then_with(|| b.cmp(&a))
                })
                .expect("a minimum exists");
            // A level whose center of gravity is its innermost room has no
            // separate hall (spec §3.1): Heart is None there.
            if heart != sanctum {
                roles[heart] = Role::Heart;
            }
        }
        // Landing: lower end of one stair AND upper end of another.
        let lowers: Vec<NodeId> = plan.stairs_into(level).iter().map(|s| s.1).collect();
        let uppers: Vec<NodeId> = plan.stairs_from(level).iter().map(|s| s.0).collect();
        for &i in &nodes {
            landing[i] = lowers.contains(&i) && uppers.contains(&i);
        }
    }

    for (i, node) in plan.nodes.iter().enumerate() {
        let mut depth = 0u8;
        let mut cur = node.realm;
        while let Some(r) = cur {
            depth = depth.saturating_add(1);
            cur = plan.realms[r].parent;
        }
        rank[i] = depth;
    }

    Reading {
        roles,
        landing,
        rank,
    }
}

/// The nodes of `level` carrying `role`, ascending by id. One for `Entry`
/// and `Sanctum`, zero or one for `Heart`, the rest for `Chamber`.
/// type-audit: bare-ok(index: level)
pub fn role_nodes(plan: &DescentPlan, reading: &Reading, level: usize, role: Role) -> Vec<NodeId> {
    plan.nodes_on(level)
        .into_iter()
        .filter(|&i| reading.roles[i] == role)
        .collect()
}

/// Sum of shortest-path hop distances from `from` to every node in `to`,
/// with paths through the WHOLE plan (stairs included). The plan is
/// connected by construction (decision 0566), so every target is reached;
/// a target that somehow is not counts as unreachable-far (`u32::MAX / 2`)
/// rather than being dropped, so a disconnected candidate can never win.
fn distance_sum(plan: &DescentPlan, from: NodeId, to: &[NodeId]) -> u32 {
    let mut seen: BTreeMap<NodeId, u32> = BTreeMap::new();
    let mut queue = VecDeque::from([from]);
    seen.insert(from, 0);
    while let Some(cur) = queue.pop_front() {
        let d = seen[&cur];
        for m in plan.neighbours(cur) {
            if let std::collections::btree_map::Entry::Vacant(e) = seen.entry(m) {
                e.insert(d + 1);
                queue.push_back(m);
            }
        }
    }
    to.iter()
        .map(|i| seen.get(i).copied().unwrap_or(u32::MAX / 2))
        .fold(0u32, |acc, d| acc.saturating_add(d))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::character::Character;
    use crate::circuit::plan_descent;
    use hornvale_kernel::{Band, Seed, Vertex};
    use hornvale_terrain::CaveKind;

    fn habitation_rungs() -> Vec<Band> {
        hornvale_terrain::rungs()
            .iter()
            .copied()
            .filter(|r| *r != Band::Surface)
            .collect()
    }

    /// The level the design was drawn against (ledger #3, the companion
    /// screen): seed 0, vertex 2, karst, DrowTier, level 1. Fifteen nodes in
    /// four within-level pieces; the Entry is n26 (depth 1), the Sanctum n38
    /// (depth 9), the Heart n5 (plan-metric distance sum 46, unique), and n5
    /// is also a landing (foot of the stair from n1, head of the stair to
    /// n42). Under a WITHIN-level metric the four-node island would have won
    /// with a sum of 4 — the defect the drawing exposed.
    #[test]
    fn the_drawn_level_reads_as_drawn() {
        let plan = plan_descent(
            Seed(0),
            Vertex(2),
            &habitation_rungs(),
            CaveKind::Karst,
            Character::DrowTier,
        );
        let r = read(&plan);
        assert_eq!(role_nodes(&plan, &r, 1, Role::Entry), vec![26]);
        assert_eq!(role_nodes(&plan, &r, 1, Role::Sanctum), vec![38]);
        assert_eq!(role_nodes(&plan, &r, 1, Role::Heart), vec![5]);
        assert!(
            r.landing[5],
            "n5 arrives by one stair and leaves by another"
        );
        assert!(!r.landing[26], "n26 is a stair foot only");
        assert_eq!(role_nodes(&plan, &r, 1, Role::Chamber).len(), 15 - 3);
    }

    /// claim: invariant(kind: [LavaTube, Fracture, Karst], character:
    /// [WildCave, DrowTier], vertex: [2], seed: 0..60) — spec §3.7's
    /// construction properties: one Entry and one Sanctum per non-empty
    /// level, never the same node; at most one Heart, never the Entry or the
    /// Sanctum; a landing has a stair in both directions; rank is 0 exactly
    /// where `realm` is `None`; `roles`, `landing` and `rank` are parallel
    /// to `plan.nodes`.
    #[test]
    fn every_level_has_one_entry_one_sanctum_and_at_most_one_heart() {
        let rungs = habitation_rungs();
        let mut hearts_seen = 0usize;
        for seed in 0..60u64 {
            for kind in [CaveKind::LavaTube, CaveKind::Fracture, CaveKind::Karst] {
                for ch in [Character::WildCave, Character::DrowTier] {
                    let plan = plan_descent(Seed(seed), Vertex(2), &rungs, kind, ch);
                    let r = read(&plan);
                    assert_eq!(r.roles.len(), plan.nodes.len());
                    assert_eq!(r.landing.len(), plan.nodes.len());
                    assert_eq!(r.rank.len(), plan.nodes.len());
                    for level in 0..rungs.len() {
                        if plan.nodes_on(level).is_empty() {
                            continue;
                        }
                        let entry = role_nodes(&plan, &r, level, Role::Entry);
                        let sanctum = role_nodes(&plan, &r, level, Role::Sanctum);
                        let heart = role_nodes(&plan, &r, level, Role::Heart);
                        assert_eq!(entry.len(), 1, "seed {seed} level {level}");
                        assert_eq!(sanctum.len(), 1, "seed {seed} level {level}");
                        assert_ne!(entry[0], sanctum[0]);
                        assert!(heart.len() <= 1, "seed {seed} level {level}");
                        if let Some(&h) = heart.first() {
                            hearts_seen += 1;
                            assert_ne!(h, entry[0]);
                            assert_ne!(h, sanctum[0]);
                        }
                    }
                    for (n, node) in plan.nodes.iter().enumerate() {
                        assert_eq!(r.rank[n] == 0, node.realm.is_none(), "node {n}");
                        if r.landing[n] {
                            let lvl = node.level as usize;
                            assert!(plan.stairs_into(lvl).iter().any(|s| s.1 == n));
                            assert!(plan.stairs_from(lvl).iter().any(|s| s.0 == n));
                        }
                    }
                }
            }
        }
        assert!(
            hearts_seen > 0,
            "a positive control: some level has a heart"
        );
    }

    /// A reading is a function of the plan alone: equal plans, equal
    /// readings; and reading the plan changes nothing about it.
    #[test]
    fn the_reading_is_pure() {
        let rungs = habitation_rungs();
        let a = plan_descent(
            Seed(7),
            Vertex(42),
            &rungs,
            CaveKind::Karst,
            Character::WildCave,
        );
        let before = format!("{a:?}");
        let r1 = read(&a);
        let r2 = read(&a);
        assert_eq!(r1.roles, r2.roles);
        assert_eq!(r1.landing, r2.landing);
        assert_eq!(r1.rank, r2.rank);
        assert_eq!(format!("{a:?}"), before, "read() does not mutate the plan");
    }
}
