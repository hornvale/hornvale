//! Gates on the descent plan (The Brattice; spec §3.1–3.4).
//!
//! A gate is a REQUIREMENT ON A WAY: what a body must hold or be to take one
//! direction of one edge. The pattern says WHERE (which side of a realm, near
//! or far); the rock and the work say WHAT (a door needs a maker, a sump
//! needs karst or fracture, a chute needs a floor below). The pass runs after
//! growth — `try_extend` splices chains into realm paths, so nothing may be
//! stamped on an edge the grammar might still rewrite — and every placement
//! is checked against solvability for a body holding nothing.
//!
//! FRAME-tier (decision 0069): derived with the plan, never serialized. The
//! one save-format consequence (spec §5) is that a descent key's IDENTITY is
//! a plan position, so a later grammar change is a real epoch.

use crate::character::Character;
use crate::circuit::{DescentPlan, EdgeKind, LengthClass, NodeId, Realm};
use hornvale_kernel::Stream;
use hornvale_terrain::CaveKind;
use std::collections::{BTreeMap, VecDeque};

/// What a body must BE to take a way (spec §3.1). Worldgen's own enum — the
/// vessel maps it onto its `MovementMode`, never the reverse (layering).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Capability {
    /// Deep water: a sump.
    Swim,
    /// The way back up a chute.
    Fly,
}

/// What a way demands.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Requirement {
    /// The key lying at this node must be held.
    Key(NodeId),
    /// The body must carry this mode.
    Mode(Capability),
}

/// One direction of one edge.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Way {
    /// Nothing asked.
    Open,
    /// A requirement.
    Needs(Requirement),
}

/// Dormans' non-conditional lock flavours — STAMPED here, realized by nothing
/// (spec §3.1). Danger is The Plat's hoarder; secrecy needs the render seam.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Hazard {
    /// The route costs something to take.
    Dangerous,
    /// The route is not known to exist.
    Uncertain,
}

/// Whether a lock stays as it is left — STAMPED, unread this campaign.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Persistence {
    /// Opened stays opened.
    Permanent,
    /// Shuts behind you (Dormans' blocked retreat).
    Collapsing,
}

/// A requirement on an edge's two ways (spec §3.1). `toward_b` is the way
/// from `Edge.a` to `Edge.b`; for a `Stair`, `a` is the upper node, so
/// `toward_b` is DOWN.
/// type-audit: bare-ok(index: pattern)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Gate {
    /// From `a` to `b` (down, on a stair).
    pub toward_b: Way,
    /// From `b` to `a` (up, on a stair).
    pub toward_a: Way,
    /// Dormans' flavour, stamped.
    pub hazard: Option<Hazard>,
    /// Stamped.
    pub persistence: Persistence,
    /// Index into [`CYCLE_PATTERNS`] of the row that placed this.
    pub pattern: usize,
}

/// A node holds the key for the gate on this edge (index into `plan.edges`).
/// type-audit: bare-ok(index: 0)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct KeyFor(pub usize);

/// Why a drawn pattern was not applied (spec §3.2 step 3–4).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Skip {
    /// No row admitted this realm's class, span, rock and work.
    Inadmissible,
    /// An edge or node the row wanted already carries a gate or key.
    Claimed,
    /// A side has no interior node where the key wants one.
    NoRoom,
    /// The default body could no longer reach the terminus or a key.
    Unsolvable,
}

/// One realm's result, in `plan.realms` order.
/// type-audit: bare-ok(index: Applied.pattern), bare-ok(index: Skipped.pattern)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Outcome {
    /// The row at `pattern` was stamped in full.
    Applied {
        /// Index into [`CYCLE_PATTERNS`].
        pattern: usize,
    },
    /// The row at `pattern` was drawn and refused.
    Skipped {
        /// Index into [`CYCLE_PATTERNS`].
        pattern: usize,
        /// Why.
        why: Skip,
    },
    /// The admissible set was empty; the draw was made and discarded.
    Inadmissible,
}

/// Which floors a realm's two paths touch.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Span {
    /// Both paths on the anchor level.
    SameFloor,
    /// `path_b` descends.
    CrossFloor,
    /// Either.
    Either,
}

/// A realm's two paths by relative length. For `LongLong`/`ShortShort`,
/// `path_a` is `Long` by convention (spec §3.2).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Side {
    /// The longer path (or `path_a` on a tie).
    Long,
    /// The shorter path (or `path_b` on a tie).
    Short,
}

/// Which end of a path, by `depth`: the shared endpoint nearer the entrance
/// is `Near`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Slot {
    /// The endpoint with the smaller depth.
    Near,
    /// The other.
    Far,
}

/// What kind of requirement a row asks for; the rock and work resolve it.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ReqKind {
    /// A door and its key — needs a maker.
    Key,
    /// A sump on a passage, a chute on a stair.
    Natural,
}

/// The shape of a gate a row places.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum WaySpec {
    /// The same requirement both ways.
    Symmetric(ReqKind),
    /// Down free, up needs — legal only on a stair (a cross-floor short
    /// side's near end).
    DownFreeUpNeeds(ReqKind),
}

/// Where a row puts a gate.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct GateSpec {
    /// Which path.
    pub side: Side,
    /// Which end of it; the gate sits on the edge adjacent to that endpoint.
    pub slot: Slot,
    /// Symmetric or asymmetric, key or natural.
    pub way: WaySpec,
}

/// Where a row puts the key: the interior node of `side` adjacent to
/// `slot`'s endpoint.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct KeySpec {
    /// Which path.
    pub side: Side,
    /// Which end.
    pub slot: Slot,
}

/// One row of the frozen inventory (spec §3.2). Data, not code: the pass
/// reads rows, never matches on names.
/// type-audit: bare-ok(identifier-text: name), bare-ok(identifier-text: source)
#[derive(Debug)]
pub struct CyclePattern {
    /// Dormans' label or the Crosscut organon's grid entry.
    pub name: &'static str,
    /// Where the row comes from.
    pub source: &'static str,
    /// Which realm classes admit it.
    pub classes: &'static [LengthClass],
    /// Which spans admit it.
    pub span: Span,
    /// Gates to place.
    pub gates: &'static [GateSpec],
    /// Key to place, if a gate wants one.
    pub key: Option<KeySpec>,
    /// A hazard stamped on every edge of one side. Row 5 (`patrol-path`)
    /// reads the spec's "hazard Dangerous on both" as a statement about the
    /// REALM: one stamp, on `path_a`, is the whole claim (Task 1 ruling).
    pub hazard: Option<(Side, Hazard)>,
    /// A persistence stamped on one edge.
    pub persistence: Option<(Side, Slot, Persistence)>,
}

use LengthClass::{LongLong, LongShort, ShortLong, ShortShort};

/// The inventory, frozen at G3 (spec §3.2). Ten rows; the count is asserted.
pub const CYCLE_PATTERNS: &[CyclePattern] = &[
    CyclePattern {
        name: "two-alternative-paths",
        source: "Dormans Fig. 9.8",
        classes: &[LongLong],
        span: Span::Either,
        gates: &[],
        key: None,
        hazard: None,
        persistence: None,
    },
    CyclePattern {
        name: "hidden-shortcut",
        source: "Dormans Fig. 9.8",
        classes: &[LongShort],
        span: Span::SameFloor,
        gates: &[],
        key: None,
        hazard: Some((Side::Short, Hazard::Uncertain)),
        persistence: None,
    },
    CyclePattern {
        name: "dangerous-route",
        source: "Dormans Fig. 9.8",
        classes: &[LongShort],
        span: Span::SameFloor,
        gates: &[],
        key: None,
        hazard: Some((Side::Short, Hazard::Dangerous)),
        persistence: None,
    },
    CyclePattern {
        name: "lock-and-key-cycle",
        source: "Dormans Fig. 9.8",
        classes: &[ShortLong],
        span: Span::SameFloor,
        gates: &[GateSpec {
            side: Side::Short,
            slot: Slot::Near,
            way: WaySpec::Symmetric(ReqKind::Key),
        }],
        key: Some(KeySpec {
            side: Side::Long,
            slot: Slot::Far,
        }),
        hazard: None,
        persistence: None,
    },
    CyclePattern {
        name: "the-sump",
        source: "Dormans, the lock-and-key aside (conditional lock)",
        classes: &[ShortLong],
        span: Span::SameFloor,
        gates: &[GateSpec {
            side: Side::Short,
            slot: Slot::Near,
            way: WaySpec::Symmetric(ReqKind::Natural),
        }],
        key: None,
        hazard: None,
        persistence: None,
    },
    CyclePattern {
        name: "patrol-path",
        source: "Dormans Fig. 9.8",
        classes: &[ShortShort],
        span: Span::SameFloor,
        gates: &[],
        key: None,
        hazard: Some((Side::Long, Hazard::Dangerous)),
        persistence: None,
    },
    CyclePattern {
        name: "blocked-retreat",
        source: "Dormans Fig. 9.8",
        classes: &[LongLong, ShortLong],
        span: Span::SameFloor,
        gates: &[],
        key: None,
        hazard: None,
        persistence: Some((Side::Long, Slot::Near, Persistence::Collapsing)),
    },
    // Ruling B (The Brattice): widened from the organon's single `LongShort`
    // entry to every cross-floor class. A drop needs a floor below, not a
    // short lower path, and that entry is structurally rare — a
    // cross-floor `path_b` is laid with at least 3 edges against a `path_a`
    // of at most 3, so `LongShort` is 20 of 4,412 realms. `name` and
    // `source` are provenance and are unchanged; the count stays 10.
    CyclePattern {
        name: "the-chute",
        source: "Crosscut organon, PREDICTED (long a / short b, two floors)",
        classes: &[LongShort, ShortLong, LongLong],
        span: Span::CrossFloor,
        gates: &[GateSpec {
            side: Side::Short,
            slot: Slot::Near,
            way: WaySpec::DownFreeUpNeeds(ReqKind::Natural),
        }],
        key: None,
        hazard: None,
        persistence: None,
    },
    CyclePattern {
        name: "key-downstairs-lock-upstairs",
        source: "Crosscut organon, PREDICTED (short a / long b, two floors)",
        classes: &[ShortLong],
        span: Span::CrossFloor,
        gates: &[GateSpec {
            side: Side::Short,
            slot: Slot::Near,
            way: WaySpec::Symmetric(ReqKind::Key),
        }],
        key: Some(KeySpec {
            side: Side::Long,
            slot: Slot::Far,
        }),
        hazard: None,
        persistence: None,
    },
    CyclePattern {
        name: "the-landing-hall",
        source: "Crosscut organon, PREDICTED (short a / short b, two floors); Alexander 133",
        classes: &[ShortShort],
        span: Span::CrossFloor,
        gates: &[],
        key: None,
        hazard: None,
        persistence: None,
    },
];

/// Which side each path is, from the class (spec §3.2: ties give `path_a`
/// Long).
pub fn sides(class: LengthClass) -> (Side, Side) {
    match class {
        LongShort | LongLong | ShortShort => (Side::Long, Side::Short),
        ShortLong => (Side::Short, Side::Long),
    }
}

fn path_of(realm: &Realm, side: Side, class: LengthClass) -> &[NodeId] {
    let (a_side, _) = sides(class);
    if side == a_side {
        &realm.path_a
    } else {
        &realm.path_b
    }
}

fn span_of(plan: &DescentPlan, realm: &Realm) -> Span {
    let anchor = realm.anchor_level;
    if realm.path_b.iter().any(|&n| plan.nodes[n].level != anchor) {
        Span::CrossFloor
    } else {
        Span::SameFloor
    }
}

/// The shared endpoints as (near, far) by `depth`; ties give `path_a[0]`
/// Near.
fn ends(plan: &DescentPlan, realm: &Realm) -> (NodeId, NodeId) {
    let s = realm.path_a[0];
    let e = *realm.path_a.last().expect("a path has two endpoints");
    if plan.nodes[e].depth < plan.nodes[s].depth {
        (e, s)
    } else {
        (s, e)
    }
}

/// The edge of `path` adjacent to the endpoint `slot` names, as `(a, b)` in
/// path order.
fn edge_at(path: &[NodeId], near: NodeId, slot: Slot) -> (NodeId, NodeId) {
    let at_start = path[0] == near;
    match (slot, at_start) {
        (Slot::Near, true) | (Slot::Far, false) => (path[0], path[1]),
        _ => (path[path.len() - 2], path[path.len() - 1]),
    }
}

/// The interior node of `path` adjacent to the endpoint `slot` names; `None`
/// if the path has no interior.
fn interior_at(path: &[NodeId], near: NodeId, slot: Slot) -> Option<NodeId> {
    if path.len() < 3 {
        return None;
    }
    let at_start = path[0] == near;
    Some(match (slot, at_start) {
        (Slot::Near, true) | (Slot::Far, false) => path[1],
        _ => path[path.len() - 2],
    })
}

fn worked(character: Character) -> bool {
    matches!(character, Character::DrowTier)
}

/// Spec §3.3: is this row's every requirement realizable in this rock and
/// work?
fn admissible(
    row: &CyclePattern,
    kind: CaveKind,
    character: Character,
    class: LengthClass,
    span: Span,
) -> bool {
    if !row.classes.contains(&class) {
        return false;
    }
    if !(row.span == Span::Either || row.span == span) {
        return false;
    }
    row.gates.iter().all(|g| match g.way {
        WaySpec::Symmetric(ReqKind::Key) => worked(character),
        WaySpec::Symmetric(ReqKind::Natural) => !matches!(kind, CaveKind::LavaTube),
        WaySpec::DownFreeUpNeeds(ReqKind::Natural) => true,
        // No row says this; refuse rather than invent.
        WaySpec::DownFreeUpNeeds(ReqKind::Key) => false,
    })
}

/// The pass (spec §3.2). One draw per realm, always; deterministic
/// thereafter.
pub(crate) fn stamp(
    plan: &mut DescentPlan,
    kind: CaveKind,
    character: Character,
    pattern_leg: &mut Stream,
    dof: &mut u32,
) {
    let realms = plan.realms.clone();
    for realm in realms.iter() {
        let span = span_of(plan, realm);
        let rows: Vec<usize> = (0..CYCLE_PATTERNS.len())
            .filter(|&i| admissible(&CYCLE_PATTERNS[i], kind, character, realm.class, span))
            .collect();
        let r = pattern_leg.next_f64();
        *dof += 1;
        if rows.is_empty() {
            plan.patterns.push(Outcome::Inadmissible);
            plan.skipped_patterns += 1;
            continue;
        }
        let pick = rows[((r * rows.len() as f64) as usize).min(rows.len() - 1)];
        match try_apply(plan, realm, pick) {
            Ok(()) => plan.patterns.push(Outcome::Applied { pattern: pick }),
            Err(why) => {
                plan.patterns.push(Outcome::Skipped { pattern: pick, why });
                plan.skipped_patterns += 1;
            }
        }
    }
    debug_assert_eq!(plan.patterns.len(), plan.realms.len());
}

fn try_apply(plan: &mut DescentPlan, realm: &Realm, pick: usize) -> Result<(), Skip> {
    let row = &CYCLE_PATTERNS[pick];
    let class = realm.class;
    let (near, _far) = ends(plan, realm);
    // Resolve everything first, then check claims, then stamp tentatively.
    let key_node = match row.key {
        Some(k) => {
            Some(interior_at(path_of(realm, k.side, class), near, k.slot).ok_or(Skip::NoRoom)?)
        }
        None => None,
    };
    let mut stamps: Vec<(usize, Gate)> = Vec::new();
    for g in row.gates {
        let (a, b) = edge_at(path_of(realm, g.side, class), near, g.slot);
        let ix = plan.edge_index(a, b).expect("a realm path edge exists");
        if plan.edges[ix].gate.is_some() {
            return Err(Skip::Claimed);
        }
        let req = match g.way {
            WaySpec::Symmetric(ReqKind::Key) | WaySpec::DownFreeUpNeeds(ReqKind::Key) => {
                Requirement::Key(key_node.expect("checked by every_key_row_places_a_key"))
            }
            WaySpec::Symmetric(ReqKind::Natural) => Requirement::Mode(Capability::Swim),
            WaySpec::DownFreeUpNeeds(ReqKind::Natural) => Requirement::Mode(Capability::Fly),
        };
        let gate = match g.way {
            WaySpec::Symmetric(_) => Gate {
                toward_b: Way::Needs(req),
                toward_a: Way::Needs(req),
                hazard: None,
                persistence: Persistence::Permanent,
                pattern: pick,
            },
            WaySpec::DownFreeUpNeeds(_) => {
                if !matches!(plan.edges[ix].kind, EdgeKind::Stair { .. }) {
                    return Err(Skip::NoRoom);
                }
                // `a` is the upper node on a Stair, so toward_b is down.
                Gate {
                    toward_b: Way::Open,
                    toward_a: Way::Needs(req),
                    hazard: None,
                    persistence: Persistence::Permanent,
                    pattern: pick,
                }
            }
        };
        stamps.push((ix, gate));
    }
    if let Some((side, hz)) = row.hazard {
        let path = path_of(realm, side, class);
        for w in path.windows(2) {
            let ix = plan.edge_index(w[0], w[1]).expect("path edge");
            if plan.edges[ix].gate.is_some() || stamps.iter().any(|(s, _)| *s == ix) {
                return Err(Skip::Claimed);
            }
            stamps.push((
                ix,
                Gate {
                    toward_b: Way::Open,
                    toward_a: Way::Open,
                    hazard: Some(hz),
                    persistence: Persistence::Permanent,
                    pattern: pick,
                },
            ));
        }
    }
    if let Some((side, slot, ps)) = row.persistence {
        let (a, b) = edge_at(path_of(realm, side, class), near, slot);
        let ix = plan.edge_index(a, b).expect("path edge");
        if plan.edges[ix].gate.is_some() || stamps.iter().any(|(s, _)| *s == ix) {
            return Err(Skip::Claimed);
        }
        stamps.push((
            ix,
            Gate {
                toward_b: Way::Open,
                toward_a: Way::Open,
                hazard: None,
                persistence: ps,
                pattern: pick,
            },
        ));
    }
    if let Some(n) = key_node
        && plan.nodes[n].key.is_some()
    {
        return Err(Skip::Claimed);
    }
    // Tentative stamp.
    for (ix, g) in &stamps {
        plan.edges[*ix].gate = Some(*g);
    }
    if let Some(n) = key_node {
        let lock_ix = stamps
            .iter()
            .find(|(_, g)| matches!(g.toward_a, Way::Needs(Requirement::Key(_))))
            .map(|(ix, _)| *ix)
            .expect("a key row has a key gate");
        plan.nodes[n].key = Some(KeyFor(lock_ix));
    }
    let reach = solvable(plan, DEFAULT_BODY);
    if reach.terminus.is_none() || reach.keys.iter().any(|k| k.is_none()) {
        for (ix, _) in &stamps {
            plan.edges[*ix].gate = None;
        }
        if let Some(n) = key_node {
            plan.nodes[n].key = None;
        }
        return Err(Skip::Unsolvable);
    }
    Ok(())
}

/// The traverser's state for the solver: modes it carries and keys it holds
/// (bit `i` = the `i`-th node of [`key_nodes`]).
/// type-audit: bare-ok(flag: swim), bare-ok(flag: fly), bare-ok(count: keys)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Body {
    /// Carries `Swim`.
    pub swim: bool,
    /// Carries `Fly`.
    pub fly: bool,
    /// Keys held, as a bitset over [`key_nodes`].
    pub keys: u64,
}

/// Walk and wade, holding nothing: the intruder (spec §3.4).
pub const DEFAULT_BODY: Body = Body {
    swim: false,
    fly: false,
    keys: 0,
};

/// Every key, every mode: the resident.
pub fn resident(plan: &DescentPlan) -> Body {
    let n = key_nodes(plan).len();
    Body {
        swim: true,
        fly: true,
        keys: if n >= 64 { u64::MAX } else { (1u64 << n) - 1 },
    }
}

/// Nodes holding a key, ascending; the bit index of each.
pub fn key_nodes(plan: &DescentPlan) -> Vec<NodeId> {
    plan.nodes
        .iter()
        .enumerate()
        .filter(|(_, n)| n.key.is_some())
        .map(|(i, _)| i)
        .collect()
}

/// What the default body — or any body — reaches (spec §3.4): shortest gated
/// distances to the terminus and to each key node, and which nodes were
/// reached at all. Product graph over `(node, keys)`; keys are only gained.
/// type-audit: bare-ok(count: terminus), bare-ok(count: keys), bare-ok(flag: reached)
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Reach {
    /// Hops to the terminus, if reachable.
    pub terminus: Option<u32>,
    /// Hops to each key node in [`key_nodes`] order.
    pub keys: Vec<Option<u32>>,
    /// Per node: reached in any state.
    pub reached: Vec<bool>,
}

fn passes(way: Way, body: Body, keys: &[NodeId]) -> bool {
    match way {
        Way::Open => true,
        Way::Needs(Requirement::Mode(Capability::Swim)) => body.swim,
        Way::Needs(Requirement::Mode(Capability::Fly)) => body.fly,
        Way::Needs(Requirement::Key(n)) => {
            let bit = keys
                .iter()
                .position(|&k| k == n)
                .expect("a key requirement names a key node");
            body.keys & (1 << bit) != 0
        }
    }
}

/// Breadth-first over the product graph from `(start, body.keys)`; returns
/// the first-visit distance per `(node, keys)`.
fn bfs(plan: &DescentPlan, start: NodeId, body: Body) -> BTreeMap<(NodeId, u64), u32> {
    let keys = key_nodes(plan);
    assert!(
        keys.len() <= 64,
        "the bitset holds 64 keys; a descent has a handful"
    );
    let mut dist: BTreeMap<(NodeId, u64), u32> = BTreeMap::new();
    let mut q = VecDeque::new();
    let mut held = body.keys;
    if let Some(bit) = keys.iter().position(|&k| k == start) {
        held |= 1 << bit;
    }
    dist.insert((start, held), 0u32);
    q.push_back((start, held));
    while let Some((n, held)) = q.pop_front() {
        let d = dist[&(n, held)];
        let here = Body { keys: held, ..body };
        for e in plan.edges.iter() {
            let (to, way) = if e.a == n {
                (e.b, e.gate.map_or(Way::Open, |g| g.toward_b))
            } else if e.b == n {
                (e.a, e.gate.map_or(Way::Open, |g| g.toward_a))
            } else {
                continue;
            };
            if !passes(way, here, &keys) {
                continue;
            }
            let mut next_held = held;
            if let Some(bit) = keys.iter().position(|&k| k == to) {
                next_held |= 1 << bit;
            }
            if dist.contains_key(&(to, next_held)) {
                continue;
            }
            dist.insert((to, next_held), d + 1);
            q.push_back((to, next_held));
        }
    }
    dist
}

/// Spec §3.4.
pub fn solvable(plan: &DescentPlan, body: Body) -> Reach {
    let keys = key_nodes(plan);
    let dist = bfs(plan, plan.entrance, body);
    let best = |n: NodeId| {
        dist.iter()
            .filter(|((m, _), _)| *m == n)
            .map(|(_, d)| *d)
            .min()
    };
    let mut reached = vec![false; plan.nodes.len()];
    for (n, _) in dist.keys() {
        reached[*n] = true;
    }
    Reach {
        terminus: best(plan.terminus),
        keys: keys.iter().map(|&k| best(k)).collect(),
        reached,
    }
}

/// Shortest entrance → terminus → entrance for `body` through the gates
/// (spec §4.2), or `None` if the terminus is unreachable.
/// type-audit: bare-ok(count: return)
pub fn gated_round_trip(plan: &DescentPlan, body: Body) -> Option<u32> {
    let out = bfs(plan, plan.entrance, body);
    let mut best: Option<u32> = None;
    for ((n, held), d) in &out {
        if *n != plan.terminus {
            continue;
        }
        let back = bfs(
            plan,
            plan.terminus,
            Body {
                keys: *held,
                ..body
            },
        );
        if let Some(r) = back
            .iter()
            .filter(|((m, _), _)| *m == plan.entrance)
            .map(|(_, d)| *d)
            .min()
        {
            let total = d + r;
            best = Some(best.map_or(total, |b| b.min(total)));
        }
    }
    best
}

/// The same round trip with every gate ignored: twice the ungated distance.
/// `Node::depth` is BFS hops from the entrance on the ungated graph, so
/// doubling it is the ungated round trip exactly.
/// type-audit: bare-ok(count: return)
pub fn ungated_round_trip(plan: &DescentPlan) -> u32 {
    2 * plan.nodes[plan.terminus].depth as u32
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::character::Character;
    use crate::circuit::{DescentPlan, LengthClass, plan_descent};
    use hornvale_kernel::{Band, Seed, Vertex};
    use hornvale_terrain::CaveKind;

    fn habitation_rungs() -> Vec<Band> {
        hornvale_terrain::rungs()
            .iter()
            .copied()
            .filter(|r| *r != Band::Surface)
            .collect()
    }
    fn plan(seed: u64, vertex: u32, kind: CaveKind, character: Character) -> DescentPlan {
        plan_descent(
            Seed(seed),
            Vertex(vertex),
            &habitation_rungs(),
            kind,
            character,
        )
    }

    #[test]
    fn the_inventory_is_frozen_at_ten_rows() {
        assert_eq!(
            CYCLE_PATTERNS.len(),
            10,
            "spec §3.2: ten rows, frozen at G3"
        );
        let mut names: Vec<&str> = CYCLE_PATTERNS.iter().map(|p| p.name).collect();
        names.sort_unstable();
        names.dedup();
        assert_eq!(names.len(), 10, "pattern names are unique");
    }

    #[test]
    fn every_key_row_places_a_key_and_no_natural_row_does() {
        for p in CYCLE_PATTERNS {
            let wants_key = p
                .gates
                .iter()
                .any(|g| matches!(g.way, WaySpec::Symmetric(ReqKind::Key)));
            assert_eq!(
                wants_key,
                p.key.is_some(),
                "{}: a Key gate needs a KeySpec and vice versa",
                p.name
            );
            let down_free = p
                .gates
                .iter()
                .any(|g| matches!(g.way, WaySpec::DownFreeUpNeeds(_)));
            if down_free {
                assert_eq!(
                    p.span,
                    Span::CrossFloor,
                    "{}: DownFreeUpNeeds is a stair's way",
                    p.name
                );
            }
        }
    }

    /// claim: invariant(seed: 0..200) — every plan on every kind × character
    /// is SOLVABLE for the default body (terminus and every key reached),
    /// and every standable node is reachable by the resident (spec §3.4, §3.8).
    #[test]
    fn every_plan_is_solvable_for_a_body_holding_nothing() {
        for seed in 0..200u64 {
            for kind in [CaveKind::LavaTube, CaveKind::Fracture, CaveKind::Karst] {
                for ch in [
                    Character::WildCave,
                    Character::FungalGardens,
                    Character::DrowTier,
                ] {
                    let p = plan(seed, 3, kind, ch);
                    let r = solvable(&p, DEFAULT_BODY);
                    assert!(
                        r.terminus.is_some(),
                        "seed {seed} {kind:?} {ch:?}: terminus unreachable"
                    );
                    assert!(
                        r.keys.iter().all(|k| k.is_some()),
                        "seed {seed} {kind:?} {ch:?}: a key is unreachable"
                    );
                    let all = solvable(&p, resident(&p));
                    assert!(
                        all.reached.iter().all(|&x| x),
                        "seed {seed}: the resident cannot reach every node"
                    );
                }
            }
        }
    }

    /// claim: invariant(seed: 0..200) — a node holds at most one key and an
    /// edge at most one gate; every gate lies on an existing edge and every
    /// key at an existing node (spec §3.8).
    #[test]
    fn gates_and_keys_claim_at_most_once_and_add_nothing() {
        for seed in 0..200u64 {
            let p = plan(seed, 1, CaveKind::Karst, Character::DrowTier);
            let keyed: Vec<_> = p.nodes.iter().filter(|n| n.key.is_some()).collect();
            for n in &keyed {
                let e = n.key.as_ref().unwrap().0;
                assert!(e < p.edges.len(), "key names a real edge");
                let g = p.edges[e]
                    .gate
                    .as_ref()
                    .expect("a key's edge carries a gate");
                assert!(
                    matches!(g.toward_a, Way::Needs(Requirement::Key(_))),
                    "a key's gate needs a key"
                );
            }
            // one key per lock and one lock per key
            let mut locks: Vec<usize> = keyed.iter().map(|n| n.key.as_ref().unwrap().0).collect();
            locks.sort_unstable();
            let before = locks.len();
            locks.dedup();
            assert_eq!(before, locks.len(), "seed {seed}: two keys for one lock");
        }
    }

    /// claim: rate(seed: 0..60) — the EXISTENCE half is the one that can go
    /// vacuous, so it is the shape: somewhere in sixty worked karst descents
    /// a `Key` gate is hung. The same sweep also carries an invariant half —
    /// a wild cave hangs a door on no seed — asserted per seed inside.
    #[test]
    fn a_worked_karst_descent_eventually_carries_a_door_and_a_wild_one_never_does() {
        let mut doors_worked = 0;
        for seed in 0..60u64 {
            let p = plan(seed, 2, CaveKind::Karst, Character::DrowTier);
            doors_worked += p
                .edges
                .iter()
                .filter(|e| {
                    matches!(
                        e.gate.as_ref().map(|g| &g.toward_a),
                        Some(Way::Needs(Requirement::Key(_)))
                    )
                })
                .count();
            let w = plan(seed, 2, CaveKind::Karst, Character::WildCave);
            assert!(
                w.edges.iter().all(|e| !matches!(
                    e.gate.as_ref().map(|g| &g.toward_a),
                    Some(Way::Needs(Requirement::Key(_)))
                )),
                "seed {seed}: a wild cave hung a door"
            );
        }
        assert!(
            doors_worked > 0,
            "sixty worked karst descents and not one door: the Key rows never apply"
        );
    }

    /// claim: rate(seed: 0..60) — again the existence half is the shape: a
    /// chute is placed somewhere in sixty lava-tube descents. The invariant
    /// half — no lava tube ever carries a sump, because dry conduit rock
    /// cannot drown a passage (spec §3.3) — is asserted per seed inside.
    #[test]
    fn a_lava_tube_never_carries_a_sump_and_some_descent_carries_a_chute() {
        let mut chutes = 0;
        for seed in 0..60u64 {
            let p = plan(seed, 2, CaveKind::LavaTube, Character::WildCave);
            for e in &p.edges {
                if let Some(g) = &e.gate {
                    assert!(
                        !matches!(g.toward_a, Way::Needs(Requirement::Mode(Capability::Swim))),
                        "seed {seed}: a sump in a lava tube"
                    );
                    if matches!(g.toward_a, Way::Needs(Requirement::Mode(Capability::Fly))) {
                        chutes += 1;
                    }
                }
            }
        }
        assert!(chutes > 0, "sixty lava-tube descents and no chute");
    }

    /// claim: invariant(seed: 0..60) — wherever a `Mode(Fly)` gate lands, it
    /// lands on a `Stair` and its down direction is `Open`. Conditional on a
    /// chute being placed at all, so it is vacuous on a slice that grows
    /// none; the existence of chutes is pinned by the sibling test above,
    /// which is why that one carries the `rate` shape and this one does not.
    #[test]
    fn a_chute_is_free_down_and_needs_flight_up_and_sits_on_a_stair() {
        for seed in 0..60u64 {
            let p = plan(seed, 4, CaveKind::Fracture, Character::WildCave);
            for e in &p.edges {
                if let Some(g) = &e.gate
                    && matches!(g.toward_a, Way::Needs(Requirement::Mode(Capability::Fly)))
                {
                    assert!(matches!(e.kind, crate::circuit::EdgeKind::Stair { .. }));
                    assert!(matches!(g.toward_b, Way::Open), "down is free");
                }
            }
        }
    }

    #[test]
    fn the_plan_and_its_gates_are_deterministic_and_read_the_vertex() {
        let a = plan(42, 1, CaveKind::Karst, Character::DrowTier);
        let b = plan(42, 1, CaveKind::Karst, Character::DrowTier);
        assert_eq!(a, b);
        let c = plan(42, 2, CaveKind::Karst, Character::DrowTier);
        assert_ne!(
            a.patterns, c.patterns,
            "two vertices draw different patterns (or the leg ignores the vertex)"
        );
    }

    #[test]
    fn length_class_gives_each_path_a_side() {
        assert_eq!(sides(LengthClass::LongShort), (Side::Long, Side::Short));
        assert_eq!(sides(LengthClass::ShortLong), (Side::Short, Side::Long));
        assert_eq!(
            sides(LengthClass::LongLong),
            (Side::Long, Side::Short),
            "tie-break: path_a is Long"
        );
        assert_eq!(sides(LengthClass::ShortShort), (Side::Long, Side::Short));
    }
}
