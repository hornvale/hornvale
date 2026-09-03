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
//!
//! The inventory is **nine** rows, not the ten the spec froze at G3: the
//! tenth, `the-landing-hall`, named a combination (`ShortShort` x cross-floor)
//! that the growth grammar cannot produce, and spec §3.2 forbids a row nothing
//! selects. The reason is GEOMETRIC, not a length count — see
//! [`CYCLE_PATTERNS`] for the lemma, and `no_cross_floor_realm_is_short_short`
//! for its witness. `no_row_is_dead_data` holds every surviving row to spec
//! §3.2's standard.

use crate::character::Character;
use crate::circuit::{DescentPlan, EdgeKind, LengthClass, NodeId, Realm};
use hornvale_kernel::Stream;
use hornvale_terrain::CaveKind;
use std::collections::{BTreeMap, BTreeSet, VecDeque};

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
    /// An edge or node the row wanted already carries a gate or key.
    Claimed,
    /// A side has no interior node where the key wants one.
    NoRoom,
    /// The default body could no longer reach the terminus, a key, OR
    /// return to the entrance (Ruling F, Task 2 fix round 1): a gate placed
    /// forward-reachable can still strand the body on the far side — a
    /// chute (down free, up needs `Fly`) into a cross-floor realm whose
    /// upper path a nested child realm blocks with a sump is a trap, not a
    /// puzzle, and `solvable`'s forward-only check never sees it.
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
    /// The path that changes floor: `path_b` of a cross-floor realm. Only
    /// meaningful with [`Span::CrossFloor`]; the drop belongs on the path
    /// that descends, whichever length it has.
    Descending,
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

/// The inventory, frozen at G3 (spec §3.2). **Nine rows; the count is
/// asserted.** The tenth, `the-landing-hall` (`ShortShort` x
/// [`Span::CrossFloor`]), was removed in execution because that combination is
/// empty by construction, and spec §3.2 forbids a row nothing selects ("a
/// pattern nothing selects is dead data, not inventory"). [`ShortShort`]
/// survives on `patrol-path`, which is same-floor.
///
/// **The lemma, stated exactly, because the obvious version of it is false.**
/// It is NOT "a cross-floor `path_b` has at least 3 edges, so both paths
/// cannot be short": `length_class(2, 3)` is `ShortShort` under the frozen
/// rule (`2 > 4` false, `3 > 3` false, `2 >= 3` false), and a 3-edge
/// cross-floor `path_b` is constructible — `try_cycle` calls `free_path(..,
/// min_interior: 0)`, so grid-adjacent landings give an empty interior. The
/// real argument is geometric, in two steps:
///
/// 1. **Both paths run between the same two grid squares.** `try_cycle`'s
///    cross-floor branch lands `lu` and `le` on the SAME squares as `u` and
///    `end` (`cu`, `ce`), so `path_b` is two stairs — which move no distance —
///    around a walk on level `ℓ+1` from `cu` to `ce`, while `path_a` is a walk
///    on level `ℓ` between those same squares. Every passage joins
///    grid-ADJACENT squares, so both are unit-step walks on one grid between
///    one pair of endpoints.
/// 2. **Therefore the two lengths share a parity.** A grid is bipartite, so
///    every walk between a fixed pair of squares has length congruent to their
///    Manhattan distance mod 2: `len_a ≡ len_b - 2 ≡ len_b (mod 2)`.
///    `ShortShort` requires `|len_a - len_b| <= 1`, which with equal parity
///    forces `len_a == len_b`; it also requires NOT both `>= 3`, hence
///    `len_a == len_b <= 2`. But `cu != ce` is a precondition of the
///    cross-floor branch, so the level-`ℓ+1` walk has at least one edge and
///    `len_b >= 3`. Contradiction.
///
/// Step 2 is what makes the lemma survive `recompute_classes`: `try_extend`
/// splices detours into realm paths after creation (`path_a` reaches 16 edges
/// against a creation ceiling of 3), and a detour is itself a unit-step walk
/// between the two squares it replaces, so it changes each length by an even
/// amount and preserves the parity the argument turns on. A creation-time case
/// analysis over `len_a in {1, 2, 3}` would prove the lemma only for the class
/// the plan no longer stores.
///
/// Witnessed by `no_cross_floor_realm_is_short_short`; every surviving row is
/// pinned live by `no_row_is_dead_data`.
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
    // Rulings B and D (The Brattice): widened from the organon's single
    // `LongShort` entry to every cross-floor class, and the gate moved from
    // `Side::Short` to `Side::Descending`. A drop needs a floor below, not a
    // short lower path, and the organon's entry is structurally rare — a
    // cross-floor `path_b` is laid with at least 3 edges against a `path_a`
    // of at most 3, so `LongShort` is 20 of 4,412 realms. Naming the side by
    // LENGTH was the defect Ruling D closes: under `ShortLong`, `sides()`
    // puts `Side::Short` on `path_a`, the same-floor existing segment, which
    // is all `Passage` edges and never a `Stair` — so 717 of the row's 823
    // draws refused with `NoRoom` while looking like inventory. `name` and
    // `source` are provenance and are unchanged.
    CyclePattern {
        name: "the-chute",
        source: "Crosscut organon, PREDICTED (long a / short b, two floors)",
        classes: &[LongShort, ShortLong, LongLong],
        span: Span::CrossFloor,
        gates: &[GateSpec {
            side: Side::Descending,
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
    // The descending side is named by its geometry, not by its length: only
    // `path_b` ever leaves the anchor level (`try_cycle`'s cross-floor
    // branch is the sole constructor that does), so a row asking for the
    // path that drops asks for `path_b` at any class.
    if side == Side::Descending {
        return &realm.path_b;
    }
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

/// Does this character's presence imply a MAKER — someone who could hang a
/// door? Exhaustive over the roster on purpose (the crate's convention, as in
/// `character::bands_of`): a sixth variant must fail to compile here rather
/// than inherit "unworked" from a wildcard and quietly lose its doors.
fn worked(character: Character) -> bool {
    match character {
        Character::DrowTier => true,
        Character::WildCave | Character::FungalGardens => false,
    }
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

/// Tentatively stamps `row` onto `realm`, then checks it is safe to keep:
/// the default body must still reach the terminus, every key, AND get back
/// to the entrance (Ruling F) — a stamp that strands the body past a gate
/// it cannot reverse is rolled back exactly like an unreachable terminus.
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
        // Both halves matter, as they do in the hazard and persistence loops
        // below: the first refuses an edge an EARLIER realm gated, the second
        // an edge THIS row already resolved to. Two `GateSpec`s of one row can
        // name one edge (a two-node path's near and far edges are the same
        // edge), and without the second check the later one would silently
        // overwrite the earlier rather than refusing.
        if plan.edges[ix].gate.is_some() || stamps.iter().any(|(s, _)| *s == ix) {
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
    let round_trip_ok = gated_round_trip(plan, DEFAULT_BODY).is_some();
    if reach.terminus.is_none() || reach.keys.iter().any(|k| k.is_none()) || !round_trip_ok {
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
/// plumb: universal(the solver's reference traverser -- walk and wade, holding nothing; the intruder every plan is proved solvable for, not any species)
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

/// Spec §4.1: from the realms whose draw selected an admissible row, to the
/// realms whose row was applied in full. `None` when no realm drew an
/// admissible row at all (every draw was [`Outcome::Inadmissible`]).
/// type-audit: bare-ok(ratio: return)
pub fn gate_yield(plan: &DescentPlan) -> Option<f64> {
    let applied = plan
        .patterns
        .iter()
        .filter(|o| matches!(o, Outcome::Applied { .. }))
        .count();
    let skipped = plan
        .patterns
        .iter()
        .filter(|o| matches!(o, Outcome::Skipped { .. }))
        .count();
    if applied + skipped == 0 {
        None
    } else {
        Some(applied as f64 / (applied + skipped) as f64)
    }
}

/// Realized requirements on the plan's edges: `(doors, sumps, chutes)`, read
/// from `toward_a` — the direction a `DownFreeUpNeeds` gate actually
/// constrains (spec §4.4).
/// type-audit: bare-ok(count: return)
pub fn realized_requirements(plan: &DescentPlan) -> (usize, usize, usize) {
    let mut doors = 0usize;
    let mut sumps = 0usize;
    let mut chutes = 0usize;
    for e in &plan.edges {
        let Some(g) = &e.gate else { continue };
        match g.toward_a {
            Way::Needs(Requirement::Key(_)) => doors += 1,
            Way::Needs(Requirement::Mode(Capability::Swim)) => sumps += 1,
            Way::Needs(Requirement::Mode(Capability::Fly)) => chutes += 1,
            Way::Open => {}
        }
    }
    (doors, sumps, chutes)
}

/// Spec §4.2: the default body's gated round trip over its ungated one,
/// over descents holding at least one realized requirement. Two things can
/// make this `None`: (1) the plan realizes no requirement at all — a round
/// trip through an entirely open graph has nothing to measure — or (2) the
/// round trip is unreachable even though a requirement exists. Since Ruling
/// F (Task 2 fix round 1), `try_apply` refuses to commit any stamp that
/// would make `gated_round_trip(plan, DEFAULT_BODY)` return `None`, so (2)
/// cannot arise from a committed plan in practice: only (1) remains
/// reachable, and that is now effectively an invariant rather than a
/// possibility this function has to guard against at read time.
/// type-audit: bare-ok(ratio: return)
pub fn detour_cost(plan: &DescentPlan) -> Option<f64> {
    let (doors, sumps, chutes) = realized_requirements(plan);
    if doors + sumps + chutes == 0 {
        return None;
    }
    let gated = gated_round_trip(plan, DEFAULT_BODY)?;
    let ungated = ungated_round_trip(plan);
    Some(gated as f64 / ungated as f64)
}

/// Spec §4.2's skip histogram, report-only: `[Inadmissible, Claimed, NoRoom,
/// Unsolvable]`, in that order.
/// type-audit: bare-ok(count: return)
pub fn skip_histogram(plan: &DescentPlan) -> [u32; 4] {
    let mut h = [0u32; 4];
    for o in &plan.patterns {
        match o {
            Outcome::Inadmissible => h[0] += 1,
            Outcome::Skipped {
                why: Skip::Claimed, ..
            } => h[1] += 1,
            Outcome::Skipped {
                why: Skip::NoRoom, ..
            } => h[2] += 1,
            Outcome::Skipped {
                why: Skip::Unsolvable,
                ..
            } => h[3] += 1,
            Outcome::Applied { .. } => {}
        }
    }
    h
}

/// A product-graph state: a node plus the key bitset held there.
type PathState = (NodeId, u64);
/// Per state: the BFS distance from the path's start, and the predecessor
/// state on a shortest path (`None` for the start itself).
type PathInfo = BTreeMap<PathState, (u32, Option<PathState>)>;

/// Shortest path over the product graph from `(start, body.keys)` to any
/// `(target, _)` state: the edge set, normalized to `(min, max)` node pairs,
/// and the keys held on arrival. Deterministic — BFS visits states in
/// nondecreasing distance, and among equal-distance arrivals at `target` the
/// smallest key bitset wins (ascending scan, replace only on strictly
/// smaller distance).
///
/// Deliberately a second BFS rather than a reuse of [`bfs`]: `bfs` records
/// only a distance per `(node, keys)` state, which is all `solvable` and
/// `gated_round_trip` ever needed, while this function additionally needs a
/// predecessor per state to reconstruct the actual path — a shape change to
/// `bfs`'s return type would ripple into every existing caller for the sake
/// of the one new one.
fn shortest_path(
    plan: &DescentPlan,
    start: NodeId,
    target: NodeId,
    body: Body,
) -> Option<(BTreeSet<(NodeId, NodeId)>, u64)> {
    let keys = key_nodes(plan);
    let mut held = body.keys;
    if let Some(bit) = keys.iter().position(|&k| k == start) {
        held |= 1 << bit;
    }
    let start_state = (start, held);
    // state -> (distance, predecessor state)
    let mut info: PathInfo = BTreeMap::new();
    info.insert(start_state, (0, None));
    let mut q = VecDeque::new();
    q.push_back(start_state);
    while let Some((n, held)) = q.pop_front() {
        let d = info[&(n, held)].0;
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
            let next_state = (to, next_held);
            if info.contains_key(&next_state) {
                continue;
            }
            info.insert(next_state, (d + 1, Some((n, held))));
            q.push_back(next_state);
        }
    }
    let mut best: Option<(PathState, u32)> = None;
    for (&state, &(d, _)) in info.iter() {
        if state.0 != target {
            continue;
        }
        match best {
            None => best = Some((state, d)),
            Some((_, bd)) if d < bd => best = Some((state, d)),
            _ => {}
        }
    }
    let (mut cur, _) = best?;
    let held_final = cur.1;
    let mut edges = BTreeSet::new();
    while let Some(prev) = info[&cur].1 {
        edges.insert((cur.0.min(prev.0), cur.0.max(prev.0)));
        cur = prev;
    }
    Some((edges, held_final))
}

/// Spec §4.4, Dormans' "unknown return path": whether the default body's
/// shortest outbound path (entrance → terminus) and shortest return path
/// (terminus → entrance, holding whatever the outbound trip gained) differ
/// as edge sets. Two things can make this `None`: (1)
/// [`realized_requirements`] is all zero — with no gate on the graph there
/// is nothing for the two directions to disagree about — or (2) the return
/// leg is unreachable even though a requirement exists. Since Ruling F
/// (Task 2 fix round 1), `try_apply` refuses to commit any stamp that would
/// strand the default body past its own gate, so (2) cannot arise from a
/// committed plan in practice: only (1) remains reachable.
/// type-audit: bare-ok(flag: return)
pub fn return_differs(plan: &DescentPlan) -> Option<bool> {
    let (doors, sumps, chutes) = realized_requirements(plan);
    if doors + sumps + chutes == 0 {
        return None;
    }
    let (out_edges, held) = shortest_path(plan, plan.entrance, plan.terminus, DEFAULT_BODY)?;
    let back_body = Body {
        keys: held,
        ..DEFAULT_BODY
    };
    let (back_edges, _) = shortest_path(plan, plan.terminus, plan.entrance, back_body)?;
    Some(out_edges != back_edges)
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
    fn the_inventory_is_frozen_at_nine_rows() {
        assert_eq!(
            CYCLE_PATTERNS.len(),
            9,
            "spec §3.2 froze ten at G3; `the-landing-hall` was removed in execution because ShortShort x CrossFloor is empty by construction — both paths run between the same two grid squares, so their lengths share a parity and cannot differ by exactly the one edge ShortShort needs (see CYCLE_PATTERNS)"
        );
        let mut names: Vec<&str> = CYCLE_PATTERNS.iter().map(|p| p.name).collect();
        names.sort_unstable();
        names.dedup();
        assert_eq!(names.len(), 9, "pattern names are unique");
        assert!(
            !names.contains(&"the-landing-hall"),
            "the removed row is back without the count moving"
        );
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
            // Ruling D: `Side::Descending` names `path_b` by its GEOMETRY,
            // and only a cross-floor realm has a path that descends. A row
            // asking for it on a same-floor span would silently resolve to
            // an ordinary passage.
            let descends = p.gates.iter().any(|g| g.side == Side::Descending)
                || p.key.is_some_and(|k| k.side == Side::Descending)
                || p.hazard.is_some_and(|(sd, _)| sd == Side::Descending)
                || p.persistence
                    .is_some_and(|(sd, _, _)| sd == Side::Descending);
            if descends {
                assert_eq!(
                    p.span,
                    Span::CrossFloor,
                    "{}: Side::Descending is only meaningful on a cross-floor realm",
                    p.name
                );
            }
        }
    }

    /// claim: invariant(seed: 0..100) — no cross-floor realm is `ShortShort`.
    /// This is the WITNESS for the lemma behind removing `the-landing-hall`
    /// (see [`CYCLE_PATTERNS`]), and it is a witness rather than a
    /// restatement because the obvious reason — "a cross-floor `path_b` has
    /// at least 3 edges, so both paths cannot be short" — is FALSE:
    /// `length_class(2, 3)` is `ShortShort`, and a 3-edge cross-floor
    /// `path_b` is constructible.
    ///
    /// The true argument is geometric. `try_cycle` lands the two lower nodes
    /// on the SAME grid squares as the realm's endpoints, so `path_b` is two
    /// distance-free stairs around a walk from `cu` to `ce` on level `ℓ+1`,
    /// while `path_a` is a walk between those same squares on level `ℓ`.
    /// Every passage joins grid-adjacent squares and a grid is bipartite, so
    /// both lengths are congruent to the endpoints' Manhattan distance mod 2
    /// — and `try_extend`'s detours, being walks between the squares they
    /// replace, preserve that. `ShortShort` needs `|len_a - len_b| <= 1` and
    /// not both `>= 3`; equal parity turns the first into `len_a == len_b`,
    /// so it needs `len_a == len_b <= 2`, while `cu != ce` forces
    /// `len_b >= 3`.
    ///
    /// If this ever reds, the lemma is wrong and `the-landing-hall` must come
    /// back — it would mean the row named a real combination after all.
    #[test]
    fn no_cross_floor_realm_is_short_short() {
        for seed in 0..100u64 {
            for kind in [CaveKind::LavaTube, CaveKind::Fracture, CaveKind::Karst] {
                for ch in [
                    Character::WildCave,
                    Character::FungalGardens,
                    Character::DrowTier,
                ] {
                    for vertex in [1u32, 5] {
                        let p = plan(seed, vertex, kind, ch);
                        for (i, r) in p.realms.iter().enumerate() {
                            let cross =
                                r.path_b.iter().any(|&n| p.nodes[n].level != r.anchor_level);
                            assert!(
                                !(cross && r.class == LengthClass::ShortShort),
                                "seed {seed} vertex {vertex} {kind:?} {ch:?} realm {i}: a cross-floor ShortShort realm exists (lens {}, {}) — the lemma is wrong and `the-landing-hall` must come back",
                                r.path_a.len() - 1,
                                r.path_b.len() - 1
                            );
                        }
                    }
                }
            }
        }
    }

    /// claim: structural(seed: 0..100) — every row of the frozen inventory is
    /// SELECTABLE: it is applied to at least one realm somewhere in the
    /// sweep. Spec §3.2's own standard ("a pattern nothing selects is dead
    /// data, not inventory"), asserted rather than assumed — four rows failed
    /// it when Task 1 first measured, which is what Rulings A, B, D and E
    /// exist to fix. A row that goes dead under a later grammar change reds
    /// here instead of sitting in a healthy-looking table.
    #[test]
    fn no_row_is_dead_data() {
        let mut applied = vec![0usize; CYCLE_PATTERNS.len()];
        for seed in 0..100u64 {
            for kind in [CaveKind::LavaTube, CaveKind::Fracture, CaveKind::Karst] {
                for ch in [
                    Character::WildCave,
                    Character::FungalGardens,
                    Character::DrowTier,
                ] {
                    for vertex in [1u32, 5] {
                        let p = plan(seed, vertex, kind, ch);
                        for o in &p.patterns {
                            if let Outcome::Applied { pattern } = o {
                                applied[*pattern] += 1;
                            }
                        }
                    }
                }
            }
        }
        let dead: Vec<&str> = CYCLE_PATTERNS
            .iter()
            .enumerate()
            .filter(|(i, _)| applied[*i] == 0)
            .map(|(_, p)| p.name)
            .collect();
        assert!(
            dead.is_empty(),
            "dead rows — never applied to any realm in the sweep: {dead:?}"
        );
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
                    assert!(
                        gated_round_trip(&p, DEFAULT_BODY).is_some(),
                        "seed {seed} {kind:?} {ch:?}: the default body cannot get back to the entrance (Ruling F)"
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

    /// claim: sanctioned-sweep(kind: [LavaTube, Fracture, Karst], character:
    /// [WildCave, FungalGardens, DrowTier], vertex: [1, 7, 42, 1000], seed:
    /// 0..400) — spec §3.8's own shape, at full size: 14,400 plans.
    ///
    /// **The same two properties
    /// [`every_plan_is_solvable_for_a_body_holding_nothing`] asserts, over
    /// 8x the surface** (The Brattice, Task 6). That test is the cheap
    /// everyday one — 1,800 plans at one vertex, seconds, in the commit
    /// gate's reach — and it stays exactly as it is; this is the sanctioned
    /// sweep §3.8 actually names, extending the Crosscut's own frozen slice
    /// (`circuit::tests::dof_counts_every_draw`'s kinds, vertices and seed
    /// range, which is why those three are copied rather than chosen) by the
    /// character axis the gate pass reads.
    ///
    /// **Why the vertex axis is the one that had to grow.** The cheap test
    /// pins vertex 3 alone, and the vertex is a seed-derivation input the
    /// pattern draw reads (`the_plan_and_its_gates_are_deterministic_and_read_the_vertex`
    /// asserts two vertices draw different patterns), so a solvability
    /// failure that only some vertices reach would be invisible to it. Four
    /// vertices x 400 seeds is what §3.8 asks for and what this runs.
    ///
    /// Four assertions per plan, all of them §3.8's:
    /// the terminus is reachable for a body holding nothing; every key is;
    /// that body can get back to the entrance (Ruling F); and the resident,
    /// who meets every requirement, reaches every node — so gates cost the
    /// resident nothing.
    ///
    /// **Cost, and where it therefore lives.** 64.7 s on the campaign Mac
    /// (`cargo test -p hornvale-worldgen --lib ... -- --ignored`, one run,
    /// 2026-09-02) against the 9.9 s its 1,800-plan sibling costs — 8x the
    /// plans, 6.5x the wall. That is well past what the commit gate may
    /// carry, so it takes the `heavy:` tag: deferred from `gate-commit`,
    /// run by the merge (decision 0426), rostered by name in
    /// `cli/tests/fixtures/heavy-roster.txt` so the addition is a visible
    /// diff rather than a tag nobody sees.
    ///
    /// The reason string is `heavy_tier.rs`'s CANONICAL one, verbatim,
    /// because that guard holds every `heavy:` reason to it character for
    /// character. Its words ("live-worldgen battery") fit this sweep only
    /// loosely — nothing here builds a world, or even a level; it is
    /// `plan_descent` and the two reachability folds over its output — but
    /// the tier is a COST class, not a subject class, and inventing a
    /// second wording to be more accurate about the subject would break the
    /// one property that makes the class greppable. The cost is stated
    /// here, where a reader of the test is, rather than in the string,
    /// which `the_canonical_heavy_reason_states_no_duration` forbids from
    /// carrying a measurement at all.
    #[test]
    #[ignore = "heavy: live-worldgen battery; deferred from the commit gate to the heavy set (decision 0132)"]
    fn every_plan_is_solvable_across_the_sanctioned_sweep() {
        for seed in 0..400u64 {
            for kind in [CaveKind::LavaTube, CaveKind::Fracture, CaveKind::Karst] {
                for ch in [
                    Character::WildCave,
                    Character::FungalGardens,
                    Character::DrowTier,
                ] {
                    for vertex in [1u32, 7, 42, 1000] {
                        let p = plan(seed, vertex, kind, ch);
                        let r = solvable(&p, DEFAULT_BODY);
                        assert!(
                            r.terminus.is_some(),
                            "seed {seed} vertex {vertex} {kind:?} {ch:?}: terminus unreachable"
                        );
                        assert!(
                            r.keys.iter().all(|k| k.is_some()),
                            "seed {seed} vertex {vertex} {kind:?} {ch:?}: a key is unreachable"
                        );
                        assert!(
                            gated_round_trip(&p, DEFAULT_BODY).is_some(),
                            "seed {seed} vertex {vertex} {kind:?} {ch:?}: the default body \
                             cannot get back to the entrance (Ruling F)"
                        );
                        let all = solvable(&p, resident(&p));
                        assert!(
                            all.reached.iter().all(|&x| x),
                            "seed {seed} vertex {vertex} {kind:?} {ch:?}: the resident cannot \
                             reach every node"
                        );
                    }
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

    // --- Task 2: readout helpers (spec §4) ---

    #[test]
    fn gate_yield_is_a_ratio_and_none_without_an_admissible_realm() {
        let p = plan(42, 1, CaveKind::Karst, Character::DrowTier);
        if let Some(y) = gate_yield(&p) {
            assert!((0.0..=1.0).contains(&y));
        }

        // A hand-built `patterns` vector, independent of the function's own
        // arithmetic (Important #2, Task 2 fix round 1): 2 Applied out of 4
        // non-Inadmissible draws, one Inadmissible excluded from both sides.
        let mut hand = p.clone();
        hand.patterns = vec![
            Outcome::Applied { pattern: 0 },
            Outcome::Skipped {
                pattern: 1,
                why: Skip::Claimed,
            },
            Outcome::Inadmissible,
            Outcome::Applied { pattern: 0 },
            Outcome::Skipped {
                pattern: 4,
                why: Skip::NoRoom,
            },
        ];
        assert_eq!(gate_yield(&hand), Some(2.0 / 4.0));

        hand.patterns = vec![Outcome::Inadmissible, Outcome::Inadmissible];
        assert_eq!(
            gate_yield(&hand),
            None,
            "every draw Inadmissible: no admissible realm at all"
        );

        hand.patterns = vec![];
        assert_eq!(gate_yield(&hand), None, "no realms at all");
    }

    /// claim: invariant(seed: 0..30) — every seed is checked against the
    /// same either/or: `None` iff no requirement is realized, `Some(r)` with
    /// `r >= 1.0` otherwise. No existence half; nothing here needs a
    /// nonvacuous count.
    #[test]
    fn detour_cost_is_at_least_one_and_none_without_a_realized_requirement() {
        for seed in 0..30u64 {
            let p = plan(seed, 1, CaveKind::Karst, Character::DrowTier);
            let (d, s, c) = realized_requirements(&p);
            match detour_cost(&p) {
                None => assert_eq!(d + s + c, 0, "seed {seed}: gates exist but no cost"),
                Some(r) => {
                    assert!(d + s + c > 0);
                    assert!(
                        r >= 1.0,
                        "seed {seed}: a gated trip shorter than the ungated one"
                    );
                }
            }
        }
    }

    #[test]
    fn a_plan_with_no_gates_has_unit_detour_and_no_return_difference() {
        let mut p = plan(7, 1, CaveKind::Karst, Character::WildCave);
        for e in &mut p.edges {
            e.gate = None;
        }
        for n in &mut p.nodes {
            n.key = None;
        }
        assert_eq!(
            gated_round_trip(&p, DEFAULT_BODY),
            Some(ungated_round_trip(&p))
        );
        assert_eq!(
            return_differs(&p),
            None,
            "no realized requirement, no reading"
        );
    }

    /// A hand-built five-node plan (Important #3, Task 2 fix round 1),
    /// following `circuit.rs`'s `bare_three_node_path()` idiom. Five nodes,
    /// in id order: `e` (0, level 0), `u` (1, level 0), `l` (2, level 1),
    /// `m` (3, level 1), `v` (4, level 0). `u` and `l` share a grid position (a
    /// stair between them); so do `m` and `v`. The only way down from `u`
    /// is the chute (`u` to `l`, down free, up needs `Fly`); the only way
    /// back up from that side is blocked without `Fly`, so the return must
    /// take the long way around: `l` to `m` (plain passage), `m` to `v`
    /// (plain stair, no gate), `v` to `u` (plain passage). Terminus is `l`,
    /// two hops down via the chute and four hops back around —
    /// deliberately unequal, so the shortest-path tie-break never has to
    /// choose between them.
    fn chute_only_descent() -> DescentPlan {
        use crate::circuit::{Edge, GridCell, Node}; // lexicon: GridCell is a lattice square, an area
        DescentPlan {
            rungs: vec![Band::Undercroft, Band::Undercroft],
            nodes: vec![
                Node {
                    level: 0,
                    cell: GridCell { col: 0, row: 0 }, // lexicon: area
                    depth: 0,
                    realm: None,
                    key: None,
                },
                Node {
                    level: 0,
                    cell: GridCell { col: 1, row: 0 }, // lexicon: area
                    depth: 1,
                    realm: None,
                    key: None,
                },
                Node {
                    level: 1,
                    cell: GridCell { col: 1, row: 0 }, // lexicon: area
                    depth: 2,
                    realm: None,
                    key: None,
                },
                Node {
                    level: 1,
                    cell: GridCell { col: 2, row: 0 }, // lexicon: area
                    depth: 3,
                    realm: None,
                    key: None,
                },
                Node {
                    level: 0,
                    cell: GridCell { col: 2, row: 0 }, // lexicon: area
                    depth: 2,
                    realm: None,
                    key: None,
                },
            ],
            edges: vec![
                // e(0) -- u(1): plain passage.
                Edge {
                    a: 0,
                    b: 1,
                    kind: EdgeKind::Passage,
                    gate: None,
                },
                // u(1, upper) -- l(2, lower): the chute. Down free, up needs Fly.
                Edge {
                    a: 1,
                    b: 2,
                    kind: EdgeKind::Stair { x: 1, y: 0 },
                    gate: Some(Gate {
                        toward_b: Way::Open,
                        toward_a: Way::Needs(Requirement::Mode(Capability::Fly)),
                        hazard: None,
                        persistence: Persistence::Permanent,
                        pattern: 0,
                    }),
                },
                // l(2) -- m(3): plain passage.
                Edge {
                    a: 2,
                    b: 3,
                    kind: EdgeKind::Passage,
                    gate: None,
                },
                // v(4, upper) -- m(3, lower): plain stair, no gate.
                Edge {
                    a: 4,
                    b: 3,
                    kind: EdgeKind::Stair { x: 2, y: 0 },
                    gate: None,
                },
                // v(4) -- u(1): plain passage, closing the level-0 loop.
                Edge {
                    a: 4,
                    b: 1,
                    kind: EdgeKind::Passage,
                    gate: None,
                },
            ],
            entrance: 0,
            terminus: 2,
            realms: vec![],
            dof: 0,
            extensions: 0,
            fallback_realms: 0,
            failed_draws: 0,
            patterns: vec![],
            skipped_patterns: 0,
        }
    }

    /// claim: structural — the positive case Important #3 asked for: a
    /// descent where the only way down is a chute has a return path that
    /// differs from its outbound, and a gated round trip strictly longer
    /// than the ungated one.
    #[test]
    fn return_differs_when_the_only_way_down_is_a_chute() {
        let p = chute_only_descent();
        assert_eq!(
            realized_requirements(&p),
            (0, 0, 1),
            "exactly one chute, no doors or sumps"
        );
        assert_eq!(return_differs(&p), Some(true));
        let gated = gated_round_trip(&p, DEFAULT_BODY);
        let ungated = ungated_round_trip(&p);
        assert_eq!(gated, Some(6), "down 2 via the chute, back 4 the long way");
        assert_eq!(ungated, 4, "2 * depth(terminus) on the ungated graph");
        assert!(
            gated.unwrap() > ungated,
            "the chute's one-way cost must show up in the round trip"
        );
    }

    /// claim: structural — remove the chute's gate and the same plan has no
    /// realized requirement, so `return_differs` reads `None` rather than a
    /// vacuous `Some(false)`.
    #[test]
    fn return_differs_is_none_on_the_chute_plan_with_its_gate_removed() {
        let mut p = chute_only_descent();
        p.edges[1].gate = None;
        assert_eq!(realized_requirements(&p), (0, 0, 0));
        assert_eq!(detour_cost(&p), None);
        assert_eq!(return_differs(&p), None);
    }
}
