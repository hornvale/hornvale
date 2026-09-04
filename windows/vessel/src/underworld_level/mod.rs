//! Underworld level generation: a chamber's own shape (The Adit).
//!
//! A chamber is a bucket, not a place (spec keystone) — `ChamberAddr`
//! addresses which of up to `BRANCHES_PER_SYSTEM` interchangeable habitats
//! exists; nothing here changes that. This module builds a second,
//! independent layer: a real room/corridor level for a rung of one cave
//! system. `FRAME`-tier under decision 0069, same as `crate::lattice`:
//! derived fresh from a `Seed` on every call, nothing serialized.

use hornvale_kernel::Seed;

use crate::lattice::{Cell, Rect};

mod carve;
mod dense;
mod region;

pub use carve::Algorithm;
pub use dense::CellGrid;

/// A cell's role within a generated underworld level.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum LevelCellKind {
    /// Standing room.
    Floor,
    /// Impassable rock.
    Wall,
    /// Standing room below the chamber's own water table (spec §4.4).
    Flooded,
    /// A connection down toward the next rung (spec §4.6).
    StairsDown,
    /// A connection up toward the rung above (spec §4.6).
    StairsUp,
    /// The one cell where a passage breaches the wall between two regions
    /// (The Brattice, spec §3.5) — EVERY passage has one, gated or not. In
    /// a cave it is a squeeze; in a building it would be a doorway. A
    /// *place*, never an object: a door is a Thing anchored here, so a
    /// `Threshold` is passable unless a shut door stands in it (§3.7).
    Threshold,
    /// Standing water too deep to wade, on a passage's RUN — the corridor
    /// cells a sump's carve turned from rock into water (The Brattice, spec
    /// §3.5). `Flooded`'s sibling one step down: you swim it.
    Deep,
    /// The lip of a chute — the vertical threshold (The Brattice, spec
    /// §3.5). You may stand at the edge, so it walks; `down` takes it, and
    /// `up` from the cell beneath it needs `Fly`.
    Drop,
}

/// How a body may move through one cell (The Gallery, Task 4; spec §3.2
/// part 3) — movement is a MODE, not a boolean. `Swim` is REACHED since The
/// Brattice: [`LevelCellKind::Deep`] answers it (spec §3.5). `Fly` is still
/// reserved — flight is a way to take an EDGE (up a chute), not a property
/// of a cell, so nothing here returns it. The same seam shape spec §3.4
/// names for reach ("one named function answers 'how', rather than a
/// constant or a boolean re-derived at every call site").
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MovementMode {
    /// Ordinary footing — dry floor, or a rung connection.
    Walk,
    /// Standing water, crossable on foot (spec §3.2 part 1: wet cells are
    /// walkable, you wade).
    Wade,
    /// Deep water: crossable only by a body that can swim (The Brattice,
    /// spec §3.5 — the mode [`LevelCellKind::Deep`] answers).
    Swim,
    /// Reserved for a sequel: flight as a travel mode.
    #[allow(dead_code)]
    Fly,
}

/// How a body may move through `kind`, or `None` if it may not move through
/// it at all — the one predicate every lateral mover asks, rather than
/// comparing against `LevelCellKind::Wall` directly (the rule
/// `CellKind::passable`'s own doc states: a rule written against the
/// variant breaks the day a new impassable kind arrives; a rule written
/// against the predicate survives it).
pub fn movement_mode(kind: LevelCellKind) -> Option<MovementMode> {
    match kind {
        LevelCellKind::Floor
        | LevelCellKind::StairsDown
        | LevelCellKind::StairsUp
        | LevelCellKind::Threshold
        | LevelCellKind::Drop => Some(MovementMode::Walk),
        LevelCellKind::Flooded => Some(MovementMode::Wade),
        LevelCellKind::Deep => Some(MovementMode::Swim),
        LevelCellKind::Wall => None,
    }
}

/// A generated underworld level: one rung of one cave system, under one
/// surface cell. Never serialized — re-derive it from the same inputs
/// rather than storing it (decision 0069).
/// type-audit: bare-ok(count: dof), bare-ok(index: thresholds)
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Level {
    /// The level's bounds.
    pub extent: Rect,
    /// Every cell of `extent`, with its kind. Total: every cell of the
    /// extent appears exactly once.
    pub cells: CellGrid,
    /// How many independent seeded choices generation made. Reported, not
    /// recomputed, mirroring `lattice::Lattice::dof`.
    pub dof: u32,
    /// Every leaf's chosen style, in generation order — Task 6 threads the
    /// realized worked-fraction from one rung's level into the next's bias.
    pub leaf_styles: Vec<LeafStyle>,
    /// Every passage's crossing cell — `(node a, node b, the divider cell)`
    /// — the underworld's `Lattice::doorways` (The Brattice, spec §3.5).
    /// One entry per `Passage` edge on this level, in `passages_on` order,
    /// gated or not: a door Thing is anchored at one of these, and a sump's
    /// crossing is `Deep` rather than `Threshold`.
    pub thresholds: Vec<(usize, usize, Cell)>,
}

/// The extent a level gets, scaled by rung (deeper rungs get more room).
/// The formula lives in `hornvale_worldgen::circuit::level_extent_wh` since
/// The Crosscut so the plan's grid and this rectangle cannot disagree.
pub fn generate_level_extent(rung: hornvale_kernel::Band) -> Rect {
    let (w, h) = hornvale_worldgen::circuit::level_extent_wh(rung);
    Rect { x: 0, y: 0, w, h }
}

/// Which content generator and worked/natural reading a leaf gets (spec
/// §4.3). The worked-vs-natural draw is independent of, but biased by, the
/// chamber's `ChamberOrigin` — decoupled on purpose, so this module never
/// writes back to a field `is_sump`'s dryness rule already depends on
/// (spec §4.4).
/// type-audit: bare-ok(flag: worked)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct LeafStyle {
    /// The content generator this leaf uses.
    pub algorithm: carve::Algorithm,
    /// Whether this leaf reads as worked stone rather than natural void.
    pub worked: bool,
}

/// The worked-chance bias used when there is no previous rung to inherit
/// from (Task 6's `generate_descent` overrides this for every rung after
/// the first).
/// type-audit: bare-ok(ratio)
/// plumb: universal(a documented neutral fallback bias used when no previous rung exists to inherit from)
pub const NEUTRAL_WORKED_BIAS: f64 = 0.5;

fn choose_leaf_style(
    cave_kind: hornvale_terrain::CaveKind,
    origin: hornvale_worldgen::chamber::ChamberOrigin,
    character: hornvale_worldgen::character::Character,
    inherited_worked_bias: f64,
    stream: &mut hornvale_kernel::Stream,
    dof: &mut u32,
) -> LeafStyle {
    use hornvale_worldgen::chamber::ChamberOrigin;
    use hornvale_worldgen::character::Character;
    let base_chance = match origin {
        ChamberOrigin::Made => 0.85,
        ChamberOrigin::Found => 0.10,
    };
    let blended = (0.5 * base_chance + 0.5 * inherited_worked_bias).clamp(0.0, 1.0);
    // The character's engine dial, applied AFTER the origin blend (The
    // Stope, Task 4; spec §A.2/A.3): a floor/cap on the final worked
    // chance, so a character's tendency survives `Found` origins instead
    // of being halved away by the blend. `CaveKind` varies WHICH natural
    // algorithm a leaf gets; the character varies HOW OFTEN a leaf is
    // worked at all — orthogonal axes, no double-variation. WildCave is
    // the identity, preserving the historical behaviour exactly.
    let worked_chance = match character {
        Character::WildCave => blended,
        // Gardens are cultivated in living caverns: never more than 35%
        // of a level reads as built.
        Character::FungalGardens => blended.min(0.35),
        // A drow-tier civilization carves: at least three fifths does.
        Character::DrowTier => blended.max(0.60),
    };
    let roll = stream.next_f64();
    *dof += 1;
    let worked = roll < worked_chance;
    let algorithm = if worked {
        carve::Algorithm::RoomsAndCorridors
    } else {
        match cave_kind {
            hornvale_terrain::CaveKind::Karst => carve::Algorithm::CellularCave,
            hornvale_terrain::CaveKind::LavaTube => carve::Algorithm::Tunneler,
            hornvale_terrain::CaveKind::Fracture => carve::Algorithm::AngularRooms,
        }
    };
    LeafStyle { algorithm, worked }
}

/// Generate a level, choosing each leaf's style from `cave_kind`/`origin`
/// with `inherited_worked_bias` folded in (Task 6's continuation draw).
/// `plan`/`level` name which rung of the plan this level realizes — its
/// nodes are this level's regions, its edges are this level's passages and
/// stairways (The Crosscut, Task 3).
///
/// **Derives one stream per algorithm family, once, before the leaf loop**
/// — not per leaf. Two leaves can share an algorithm (composite levels are
/// the point), so `carve`'s own doc explains why a shared, advancing
/// stream is required rather than a fresh derive per call: the same
/// `derive once, thread &mut Stream through every draw` shape
/// `lattice::allocate`/`lattice::grow` already use.
/// type-audit: bare-ok(ratio: inherited_worked_bias), bare-ok(index: level)
// Eight arguments because a level is realized from eight independent
// givens — extent, rock, origin, character, the inherited worked bias, the
// plan, which rung of it, and the seed. Bundling them into a struct would
// only rename the same eight at every call site.
#[allow(clippy::too_many_arguments)]
pub fn generate_level_with_origin(
    extent: Rect,
    cave_kind: hornvale_terrain::CaveKind,
    origin: hornvale_worldgen::chamber::ChamberOrigin,
    character: hornvale_worldgen::character::Character,
    inherited_worked_bias: f64,
    plan: &hornvale_worldgen::circuit::DescentPlan,
    level: usize,
    seed: Seed,
) -> Level {
    debug_assert_eq!(
        extent,
        generate_level_extent(plan.rungs[level]),
        "extent must be the rung's own"
    );
    let mut cells = CellGrid::new(extent, LevelCellKind::Wall);
    let mut dof = 0u32;
    let mut style_stream = seed.derive(crate::streams::UNDERWORLD_LEVEL_STYLE).stream();
    let mut cellular_stream = seed
        .derive(crate::streams::UNDERWORLD_LEVEL_CELLULAR)
        .stream();
    let mut tunneler_stream = seed
        .derive(crate::streams::UNDERWORLD_LEVEL_TUNNELER)
        .stream();
    let mut rooms_stream = seed.derive(crate::streams::UNDERWORLD_LEVEL_ROOMS).stream();
    let mut leaf_styles = Vec::new();
    let node_ids = plan.nodes_on(level);
    for &id in &node_ids {
        let rect = rect_of(plan, id);
        let style = choose_leaf_style(
            cave_kind,
            origin,
            character,
            inherited_worked_bias,
            &mut style_stream,
            &mut dof,
        );
        let stream = match style.algorithm {
            carve::Algorithm::CellularCave => &mut cellular_stream,
            carve::Algorithm::Tunneler => &mut tunneler_stream,
            carve::Algorithm::AngularRooms | carve::Algorithm::RoomsAndCorridors => {
                &mut rooms_stream
            }
        };
        dof += carve::carve(style.algorithm, rect, stream, &mut cells);
        ensure_standable(rect, &mut cells);
        leaf_styles.push(style);
    }
    // Passages: one L-corridor between the nearest walkable pair of each
    // connected region pair. Two grid-adjacent regions with NO plan edge
    // keep their wall — the non-adjacency the partition tree could never say.
    // Each one also realizes its gate's PLACE (The Brattice, spec §3.5): the
    // single L cell lying on the divider between the two rects becomes a
    // `Threshold`, or — if the plan stamped a sump on this edge — the whole
    // stretch of the L that was rock a moment ago becomes `Deep`.
    let mut thresholds = Vec::new();
    for (a, b) in plan.passages_on(level) {
        let ra = rect_of(plan, a);
        let rb = rect_of(plan, b);
        let ca = walkable_cells_in_rect(ra, &cells);
        let cb = walkable_cells_in_rect(rb, &cells);
        if let Some((pa, pb)) = nearest_pair(&ca, &cb) {
            let l_cells = l_corridor(pa, pb);
            // The corridor PROPER: what the carve is about to turn from
            // rock into a way through. Never the two walkable endpoints
            // inside the regions, and never any floor the L happens to run
            // along — so a `Deep` cell was rock a moment earlier and no
            // walker's connectivity WITHIN a region changes, only the way
            // between the two.
            let rock_before: Vec<Cell> = l_cells
                .iter()
                .copied()
                .filter(|c| cells.get(*c) == Some(LevelCellKind::Wall))
                .collect();
            // PLAIN `connect_cells`, not the way-preserving one: a later
            // passage's L may pave an EARLIER passage's `Threshold` or a
            // stretch of its `Deep` run, and that is the chosen behaviour
            // here rather than an oversight. A passage's job is to leave
            // walkers connected, and paving only ever makes a cell MORE
            // passable; preserving instead would let one edge's gate stand
            // in the middle of another edge's corridor, which is a
            // requirement no plan stamped. What it must not do is destroy a
            // gate outright, and that is not asserted by this loop but
            // WITNESSED: the realization witness counts `Threshold` against
            // non-sump passages and `Deep` against sumps in both
            // directions, so an edge whose crossing was paved away goes red
            // there. A stair FOOT's connector is the opposite case and
            // takes the opposite rule — see `is_placed_way_for_a_foot`.
            connect_cells(pa, pb, &mut cells);
            // The crossing: the one L cell in neither rect. `region_rect`
            // keeps a one-cell dividing wall on every side, and an L between
            // two grid-adjacent regions crosses that line exactly once —
            // asserted here rather than assumed (spec §3.5).
            let crossing: Vec<Cell> = l_cells
                .iter()
                .copied()
                .filter(|c| !ra.contains(*c) && !rb.contains(*c))
                .collect();
            debug_assert_eq!(
                crossing.len(),
                1,
                "an L between grid-adjacent regions crosses the divider once: \
                 {a}-{b} crossed at {crossing:?}"
            );
            if let Some(&cross) = crossing.first() {
                if is_sump(plan, a, b) {
                    // One cell has one kind, so the crossing is DEEP here
                    // rather than a threshold — a drowned squeeze. The
                    // execution amendment to spec §3.5 recorded in the
                    // realization witness's own doc.
                    for c in rock_before {
                        cells.set(c, LevelCellKind::Deep);
                    }
                } else {
                    cells.set(cross, LevelCellKind::Threshold);
                }
                thresholds.push((a, b, cross));
            }
        }
    }
    // Stairs: the plan's shared coordinate, made standable and joined to its
    // region if the carve left it in rock. A chute (spec §3.5) writes a
    // `Drop` lip up here and NO `StairsUp` below — its landing is made
    // standable `Floor` and its region reconnected exactly as a stair foot
    // is, which is the whole of the asymmetry.
    for (upper, lower, x, y) in plan.stairs_from(level) {
        let kind = if is_chute(plan, upper, lower) {
            LevelCellKind::Drop
        } else {
            LevelCellKind::StairsDown
        };
        place_stair(Cell(x, y), kind, rect_of(plan, upper), &mut cells);
    }
    let mut landings: Vec<Cell> = Vec::new();
    for (upper, lower, x, y) in plan.stairs_into(level) {
        if is_chute(plan, upper, lower) {
            place_stair(
                Cell(x, y),
                LevelCellKind::Floor,
                rect_of(plan, lower),
                &mut cells,
            );
            landings.push(Cell(x, y));
        } else {
            place_stair(
                Cell(x, y),
                LevelCellKind::StairsUp,
                rect_of(plan, lower),
                &mut cells,
            );
        }
    }
    // The deepest level's terminus keeps today's dangling stairs down, so
    // `STAIRS_LEAD_NOWHERE_REFUSAL` keeps its one firing case (spec §3.1).
    // `reconnect_region` repairs the same articulation-point hazard
    // `place_stair` guards above, for the one stair this loop does not
    // place through it.
    if level + 1 == plan.rungs.len() {
        let rect = rect_of(plan, plan.terminus);
        // Never consume a chute's landing: the witness (spec §3.5) requires
        // a `Drop` to sit over a standable NON-stair cell, and a landing
        // that is also this region's first walkable cell would otherwise be
        // overwritten with the dangling terminus stair.
        let free = walkable_cells_in_rect(rect, &cells)
            .into_iter()
            .find(|c| !landings.contains(c));
        let choice = free.or_else(|| first_walkable_cell_in(rect, &cells));
        // Both `debug_assert`s NAME A CAUSE rather than leaving a silent
        // skip to be traced back here — The Crosscut's own deferred minor,
        // taken now that this write is being touched. The first is that
        // minor exactly (the region's footing is entirely stair landings,
        // so `first_walkable_cell_in` finds nothing and the terminus
        // stairway is dropped); the second is The Brattice's new sibling of
        // it (the only footing left is a chute's landing, which this write
        // is about to overwrite).
        debug_assert!(
            choice.is_some() || standable_cells_in_rect(rect, &cells).is_empty(),
            "the terminus stairway is being skipped: region {rect:?} has standable \
             cells but no Floor/Flooded one — its only footing is already a stair \
             landing"
        );
        debug_assert!(
            free.is_some() || walkable_cells_in_rect(rect, &cells).is_empty(),
            "the terminus region {rect:?} has no walkable cell that is not a chute \
             landing; the terminus stair is about to overwrite one"
        );
        if let Some(c) = choice {
            cells.set(c, LevelCellKind::StairsDown);
            // Scoped to the terminus's OWN region, never wider: a repair
            // that reached outside `rect` could bridge into a
            // grid-adjacent region the plan deliberately left unlinked,
            // carving straight through the wall spec §3.3 promises stays
            // solid there.
            reconnect_region(rect, &mut cells);
        }
    }
    Level {
        extent,
        cells,
        dof,
        leaf_styles,
        thresholds,
    }
}

/// Whether the plan stamped a sump on the edge between `a` and `b` — a
/// passage whose way demands `Swim` (The Brattice, spec §3.5). Reads
/// `toward_a`, the canonical field: a sump needs the mode in both
/// directions, and for a `Stair` `toward_a` is the way UP.
fn is_sump(
    plan: &hornvale_worldgen::circuit::DescentPlan,
    a: hornvale_worldgen::circuit::NodeId,
    b: hornvale_worldgen::circuit::NodeId,
) -> bool {
    matches!(
        plan.gate_between(a, b).map(|(_, g)| g.toward_a),
        Some(hornvale_worldgen::brattice::Way::Needs(
            hornvale_worldgen::brattice::Requirement::Mode(
                hornvale_worldgen::brattice::Capability::Swim
            )
        ))
    )
}

/// Whether the plan stamped a chute on the stairway between `upper` and
/// `lower` — a way down that is free and a way back up that demands `Fly`
/// (The Brattice, spec §3.5).
fn is_chute(
    plan: &hornvale_worldgen::circuit::DescentPlan,
    upper: hornvale_worldgen::circuit::NodeId,
    lower: hornvale_worldgen::circuit::NodeId,
) -> bool {
    matches!(
        plan.gate_between(upper, lower).map(|(_, g)| g.toward_a),
        Some(hornvale_worldgen::brattice::Way::Needs(
            hornvale_worldgen::brattice::Requirement::Mode(
                hornvale_worldgen::brattice::Capability::Fly
            )
        ))
    )
}

/// The plan's region rectangle as the lattice's `Rect`.
fn rect_of(
    plan: &hornvale_worldgen::circuit::DescentPlan,
    node: hornvale_worldgen::circuit::NodeId,
) -> Rect {
    let r = plan.region_of(node);
    Rect {
        x: r.x,
        y: r.y,
        w: r.w,
        h: r.h,
    }
}

/// Every `Floor`/`Flooded` cell inside `rect`, ascending `(x, y)`.
fn walkable_cells_in_rect(rect: Rect, cells: &CellGrid) -> Vec<Cell> {
    let mut out = Vec::new();
    for x in rect.x..(rect.x + rect.w) {
        for y in rect.y..(rect.y + rect.h) {
            let cell = Cell(x, y);
            if matches!(
                cells.get(cell),
                Some(LevelCellKind::Floor) | Some(LevelCellKind::Flooded)
            ) {
                out.push(cell);
            }
        }
    }
    out
}

/// The first `Floor`/`Flooded` cell in `rect`, column-major.
fn first_walkable_cell_in(rect: Rect, cells: &CellGrid) -> Option<Cell> {
    walkable_cells_in_rect(rect, cells).into_iter().next()
}

/// Every `Floor`/`Flooded`/`StairsDown`/`StairsUp`/`Threshold`/`Drop` cell
/// inside `rect` — footing, in the repair's own sense —
/// `reconnect_region`'s OWN notion of "already standable", broader than
/// `walkable_cells_in_rect`'s Floor/Flooded-only set on purpose: a stair
/// `place_stair` just carved is itself walkable (`movement_mode` answers
/// `Walk` for it), so a one-wide corridor with a stair in its middle is
/// already ONE component, not two either side of a severance. Using the
/// narrower Floor/Flooded set here would read that stair as a cut and
/// carve a redundant bypass around a cell that was never actually
/// blocking anything. The Brattice adds `Threshold` and `Drop` for the same
/// reason (spec §3.5): both walk, so neither is a severance. `Deep` is
/// deliberately NOT here — it swims, and a repair must not treat deep water
/// as footing. The passage-connection code and `place_stair`'s own
/// walkable-cell join keep using `walkable_cells_in_rect` unchanged — this
/// predicate is only for deciding whether a region needs repairing.
fn standable_cells_in_rect(rect: Rect, cells: &CellGrid) -> Vec<Cell> {
    let mut out = Vec::new();
    for x in rect.x..(rect.x + rect.w) {
        for y in rect.y..(rect.y + rect.h) {
            let cell = Cell(x, y);
            if matches!(
                cells.get(cell),
                Some(LevelCellKind::Floor)
                    | Some(LevelCellKind::Flooded)
                    | Some(LevelCellKind::StairsDown)
                    | Some(LevelCellKind::StairsUp)
                    | Some(LevelCellKind::Threshold)
                    | Some(LevelCellKind::Drop)
            ) {
                out.push(cell);
            }
        }
    }
    out
}

/// A region always has somewhere to stand: if a carve left `rect` all rock,
/// open its centre cell. A guarantee, so no downstream step (passages,
/// stairs, the entrance) can find an empty region.
fn ensure_standable(rect: Rect, cells: &mut CellGrid) {
    if first_walkable_cell_in(rect, cells).is_none() {
        cells.set(
            Cell(rect.x + rect.w / 2, rect.y + rect.h / 2),
            LevelCellKind::Floor,
        );
    }
}

/// Cut a stairs cell at `at` and, if the carve had left that cell in rock,
/// join it to the nearest walkable cell of its own region so a landing is
/// never sealed off. A stairway has a foot (spec §3.3).
///
/// **The connector never paves over a way already placed** — see
/// [`is_placed_way`] for the defect that rule closes.
///
/// **`kind` is not required to be a stair.** The Brattice writes a chute's
/// lip through here as [`LevelCellKind::Drop`] and its landing as
/// [`LevelCellKind::Floor`] (spec §3.5) — the join and the repair below are
/// what this function is FOR, and both are exactly as wanted for a chute;
/// nothing in the body ever looked at which variant it was handed.
///
/// **Converting an existing `Floor`/`Flooded` cell can strand the rest of
/// its own region** if that cell was an articulation point (a Karst seed's
/// carve left a one-cell-wide corridor, and overwriting its middle cell
/// marooned four cells beyond it). `reconnect_region` repairs this, scoped
/// to `region` and `region` ALONE — never `extent` — after every write:
/// spec §3.3's whole point is that two grid-adjacent regions with no plan
/// edge keep a solid wall between them, so a repair that reached outside
/// `region` could bridge exactly the non-adjacency the plan deliberately
/// left unlinked. Cheap (one BFS over one region) and a no-op whenever the
/// cell was not a bridge.
fn place_stair(at: Cell, kind: LevelCellKind, region: Rect, cells: &mut CellGrid) {
    let was_walkable = matches!(
        cells.get(at),
        Some(LevelCellKind::Floor) | Some(LevelCellKind::Flooded)
    );
    if !was_walkable {
        let walkable = walkable_cells_in_rect(region, cells);
        if let Some((_, target)) = nearest_pair(&[at], &walkable) {
            connect_cells_preserving_ways(at, target, cells);
        }
    }
    cells.set(at, kind);
    reconnect_region(region, cells);
}

/// If overwriting one cell split `region`'s own standable cells (Floor,
/// Flooded, AND any stair already carved — see `standable_cells_in_rect`)
/// into more than one component, stitch every extra component back to the
/// first by routing AROUND the blocking cell — `shortest_route_within_rect`
/// walks `region` alone, including rock, and never steps onto an existing
/// stair, so it finds a detour even where a straight line between the two
/// nearest cells would have to cross back over the very cell that caused
/// the split. Bounded to `region`: a repair that carved outside it could
/// bridge a grid-adjacent region the plan left deliberately unlinked (spec
/// §3.3 — no passage edge means no way through, ever). A no-op whenever
/// `region` was already one component (the common case).
fn reconnect_region(region: Rect, cells: &mut CellGrid) {
    loop {
        let remaining = standable_cells_in_rect(region, cells);
        if remaining.len() < 2 {
            return;
        }
        let all: std::collections::BTreeSet<Cell> = remaining.iter().copied().collect();
        let mut seen = std::collections::BTreeSet::new();
        let mut queue = std::collections::VecDeque::new();
        seen.insert(remaining[0]);
        queue.push_back(remaining[0]);
        while let Some(Cell(x, y)) = queue.pop_front() {
            for (dx, dy) in [(1, 0), (-1, 0), (0, 1), (0, -1)] {
                let next = Cell(x + dx, y + dy);
                if all.contains(&next) && seen.insert(next) {
                    queue.push_back(next);
                }
            }
        }
        if seen.len() == remaining.len() {
            return;
        }
        let targets: std::collections::BTreeSet<Cell> = remaining
            .iter()
            .copied()
            .filter(|c| !seen.contains(c))
            .collect();
        // Route AROUND the stair rather than across it: `connect_cells`'s
        // straight L can only fail to bridge because the stair sits ON that
        // exact L, which is precisely the case that needs a detour, not a
        // second attempt at the same line — a BFS over the whole region,
        // through rock if need be, is the general fix `nearest_pair` alone
        // could not give.
        match shortest_route_within_rect(region, cells, &seen, &targets) {
            Some(path) => {
                for c in path {
                    if !is_placed_way(cells.get(c)) {
                        cells.set(c, LevelCellKind::Floor);
                    }
                }
            }
            // No route exists within the rect at all (the stair severed a
            // corridor exactly one cell wide, with no room to go around) —
            // stop rather than loop forever retrying an impossible repair.
            None => return,
        }
    }
}

/// BFS over every cell of `region` (never leaving it, never stepping onto
/// an existing stair) from any of `sources`, returning the shortest path
/// (inclusive of both ends) to the nearest cell in `targets`, or `None` if
/// no such route exists within the rect.
fn shortest_route_within_rect(
    region: Rect,
    cells: &CellGrid,
    sources: &std::collections::BTreeSet<Cell>,
    targets: &std::collections::BTreeSet<Cell>,
) -> Option<Vec<Cell>> {
    let mut parent: std::collections::BTreeMap<Cell, Cell> = std::collections::BTreeMap::new();
    let mut seen: std::collections::BTreeSet<Cell> = sources.clone();
    let mut queue: std::collections::VecDeque<Cell> = sources.iter().copied().collect();
    while let Some(cur) = queue.pop_front() {
        if targets.contains(&cur) {
            let mut path = vec![cur];
            let mut at = cur;
            while let Some(&p) = parent.get(&at) {
                path.push(p);
                at = p;
            }
            path.reverse();
            return Some(path);
        }
        let Cell(x, y) = cur;
        for (dx, dy) in [(1, 0), (-1, 0), (0, 1), (0, -1)] {
            let next = Cell(x + dx, y + dy);
            if !region.contains(next) {
                continue;
            }
            // Never route THROUGH a gate's own place: a repair that carved
            // around a door would defeat it, exactly as a repair that carved
            // through a wall defeated spec §3.3 (The Brattice, spec §3.5).
            if is_placed_way(cells.get(next)) {
                continue;
            }
            if seen.insert(next) {
                parent.insert(next, cur);
                queue.push_back(next);
            }
        }
    }
    None
}

/// The closest pair of cells (Manhattan distance) between two sets —
/// O(len(a) * len(b)), fine for a one-time generation step at level-sized
/// cell counts.
fn nearest_pair(a: &[Cell], b: &[Cell]) -> Option<(Cell, Cell)> {
    let mut best: Option<(Cell, Cell, i32)> = None;
    for &pa in a {
        for &pb in b {
            let dist = (pa.0 - pb.0).abs() + (pa.1 - pb.1).abs();
            if best.is_none_or(|(_, _, d)| dist < d) {
                best = Some((pa, pb, dist));
            }
        }
    }
    best.map(|(pa, pb, _)| (pa, pb))
}

/// Whether a cell is a WAY ALREADY PLACED — a stair, or one of the gate
/// places The Brattice writes (spec §3.5) — and so must never be paved over
/// by a corridor carve or a repair, nor routed through by one.
///
/// The two REPAIR sites share this one predicate deliberately
/// ([`reconnect_region`], [`shortest_route_within_rect`]); the stair-foot
/// connector takes the sibling predicate [`is_placed_way_for_a_foot`],
/// which drops `Deep` for the reason Ruling G gives there. They were two
/// lists and a third that
/// had no list at all, and the missing one was a live defect — a stair's
/// own connector L, carved to give a LATER stair its foot, paved straight
/// over an EARLIER stair on the same region, leaving an orphan `StairsUp`
/// on the rung below with nothing above it.
///
/// **Measured at the commit before this one**, not inferred: a probe over
/// Karst/`DrowTier`, vertex 1, seeds `0..200` found **23 broken stairways**,
/// the first being seed 10, level 1, where the stairway `36 -> 40` at
/// `(2, 2)` had been paved back to `Floor` while its `StairsUp` stood on
/// the rung below. It went unobserved because
/// `stairs_pair_by_coordinate_across_adjacent_rungs` sweeps
/// `Fracture`/`WildCave` alone; the realization witness sweeps three
/// `(kind, character)` pairs and its stair half is what pins this now.
/// `circuit.rs::stair_coordinate`
/// already forbids two stairways SHARING a coordinate; nothing forbade one
/// stairway's corridor from crossing another's cell. Preserving it costs
/// nothing: every kind named here is itself passable, so the corridor still
/// arrives.
fn is_placed_way(kind: Option<LevelCellKind>) -> bool {
    matches!(
        kind,
        Some(LevelCellKind::StairsDown)
            | Some(LevelCellKind::StairsUp)
            | Some(LevelCellKind::Threshold)
            | Some(LevelCellKind::Deep)
            | Some(LevelCellKind::Drop)
    )
}

/// [`is_placed_way`], minus `Deep` — the predicate a STAIR FOOT's connector
/// uses (**Ruling G**).
///
/// A foot connector's whole job is to give a stair, a chute lip or a chute
/// landing a walkable join to its own region. Preserving `Deep` here would
/// leave a swim in the middle of that join: the stair would stand behind a
/// `Swim` requirement **the plan never stamped**, and both connectivity
/// sweeps would miss it, because both were widened to
/// `movement_mode(..).is_some()` — which `Deep` satisfies. So the foot
/// connector PAVES through deep water.
///
/// The sump survives that paving, which is why the ruling is safe: a foot
/// connector runs between two cells of ONE region's rect and therefore
/// never reaches the divider, so the sump's CROSSING cell — the one that
/// makes the gate a gate — is out of its reach. Only the run's
/// inside-the-region tail is shortened. The two repair sites keep
/// [`is_placed_way`] and still never overwrite `Deep`: they are not making
/// a foot, they are re-stitching a region, and there a sump's run is
/// scenery to route around rather than an obstacle in the way of a
/// stairway.
///
/// Witnessed, not asserted: the realization witness's walker arm BFSes from
/// every stair, lip and landing over `Walk`/`Wade` cells alone, confined to
/// the cell's own region, and requires it to reach ordinary footing.
fn is_placed_way_for_a_foot(kind: Option<LevelCellKind>) -> bool {
    is_placed_way(kind) && kind != Some(LevelCellKind::Deep)
}

/// [`connect_cells`], but leaving every [`is_placed_way_for_a_foot`] cell
/// as it is — see that predicate for why `Deep` is the one placed way a
/// stair foot's connector is allowed to pave (Ruling G).
fn connect_cells_preserving_ways(a: Cell, b: Cell, cells: &mut CellGrid) {
    for c in l_corridor(a, b) {
        if !is_placed_way_for_a_foot(cells.get(c)) {
            cells.set(c, LevelCellKind::Floor);
        }
    }
}

/// Carve a straight L-shaped `Floor` corridor between two arbitrary
/// cells — the same shape `carve::connect_centers` already uses for
/// within-leaf room connections, generalized to take endpoints directly
/// rather than deriving them from room rects.
fn connect_cells(a: Cell, b: Cell, cells: &mut CellGrid) {
    for c in l_corridor(a, b) {
        cells.set(c, LevelCellKind::Floor);
    }
}

/// The cells [`connect_cells`] writes, in the order it writes them —
/// extracted so a caller that needs to know WHICH cells an L touches (The
/// Brattice's threshold and sump placement, spec §3.5) cannot disagree with
/// the carve itself. The horizontal leg first, then the vertical: the
/// corner `(b.0, a.1)` appears in both, exactly as the two loops always
/// overwrote it, so the set of cells and the resulting grid are unchanged.
fn l_corridor(a: Cell, b: Cell) -> Vec<Cell> {
    let mut out = Vec::new();
    for x in a.0.min(b.0)..=a.0.max(b.0) {
        out.push(Cell(x, a.1));
    }
    for y in a.1.min(b.1)..=a.1.max(b.1) {
        out.push(Cell(b.0, y));
    }
    out
}

/// Generate a level over `extent` for one rung of a single-rung
/// [`hornvale_worldgen::circuit::DescentPlan`], test-and-example helper.
///
/// **Delegates to `generate_level_with_origin`** with a fixed default
/// kind/origin — this simple entry point's own callers are the only thing
/// that default matters for; the integration tests exercise real
/// `Chamber` values through `generate_level_with_origin` directly.
/// `extent` MUST equal `generate_level_extent(Band::Undercroft)`
/// (debug-asserted by the callee).
pub fn generate_level(extent: Rect, seed: Seed) -> Level {
    let plan = hornvale_worldgen::circuit::plan_descent(
        seed,
        hornvale_kernel::Vertex(0),
        &[hornvale_kernel::Band::Undercroft],
        hornvale_terrain::CaveKind::Karst,
        hornvale_worldgen::character::Character::WildCave,
    );
    generate_level_with_origin(
        extent,
        hornvale_terrain::CaveKind::Karst,
        hornvale_worldgen::chamber::ChamberOrigin::Found,
        hornvale_worldgen::character::Character::WildCave,
        NEUTRAL_WORKED_BIAS,
        &plan,
        0,
        seed,
    )
}

/// As `generate_level_with_origin`, additionally flooding every UNWORKED
/// leaf's `Floor` cells when `depth_m`/`water_table_m` says this level sits
/// in the phreatic zone (The Gallery, Task 4; spec §3.2's water rule,
/// rewritten at the Task 0 stop).
///
/// **Wetness keys on `LeafStyle.worked`, not on `origin`.** The rule is:
/// wet is common and correct (a worked leaf is drained — cut and kept dry
/// by whoever built it — a natural leaf is wet), and it applies per LEAF,
/// not per chamber. `origin`'s own `is_sump` short-circuit
/// (`hornvale_worldgen::chamber::is_sump` returns `false` for
/// `ChamberOrigin::Made` unconditionally) never actually reaches a leaf's
/// dryness under this rule, because the shipped path
/// (`Underground::enter`) only ever produces `Found` — so this reads
/// `hornvale_terrain::is_phreatic` directly, the physical half of
/// `is_sump`'s own definition, and lets each leaf's own `worked` flag (not
/// the chamber-wide origin) decide whether IT floods.
///
/// **The alignment this depends on**: `generate_level_with_origin` builds
/// `leaf_styles` in one pass over `plan.nodes_on(level)`, pushing one style
/// per node in the same order — so `level.leaf_styles[i]` is always
/// `plan.nodes_on(level)[i]`'s own style, and re-deriving that node list
/// here from the same `plan`/`level` (rather than threading it through,
/// `FRAME`-tier re-derivation being exactly what decision 0069 calls for)
/// is safe to zip against it.
/// type-audit: bare-ok(diagnostic-value: depth_m), bare-ok(diagnostic-value: water_table_m), bare-ok(ratio: inherited_worked_bias), bare-ok(index: level)
#[allow(clippy::too_many_arguments)]
pub fn generate_level_with_water(
    extent: Rect,
    cave_kind: hornvale_terrain::CaveKind,
    origin: hornvale_worldgen::chamber::ChamberOrigin,
    character: hornvale_worldgen::character::Character,
    depth_m: f64,
    water_table_m: f64,
    inherited_worked_bias: f64,
    plan: &hornvale_worldgen::circuit::DescentPlan,
    level: usize,
    seed: Seed,
) -> Level {
    let mut lvl = generate_level_with_origin(
        extent,
        cave_kind,
        origin,
        character,
        inherited_worked_bias,
        plan,
        level,
        seed,
    );
    if hornvale_terrain::is_phreatic(depth_m, water_table_m) {
        let rects: Vec<Rect> = plan
            .nodes_on(level)
            .iter()
            .map(|&id| rect_of(plan, id))
            .collect();
        debug_assert_eq!(
            rects.len(),
            lvl.leaf_styles.len(),
            "nodes_on(level) and leaf_styles must stay index-aligned"
        );
        for (rect, style) in rects.iter().zip(lvl.leaf_styles.iter()) {
            if style.worked {
                // Drained by whoever cut it — the same reading `is_sump`
                // already gives `ChamberOrigin::Made`, now applied per leaf
                // rather than per chamber.
                continue;
            }
            for x in rect.x..(rect.x + rect.w) {
                for y in rect.y..(rect.y + rect.h) {
                    let cell = Cell(x, y);
                    if lvl.cells.get(cell) == Some(LevelCellKind::Floor) {
                        lvl.cells.set(cell, LevelCellKind::Flooded);
                    }
                }
            }
        }
    }
    lvl
}

/// The realized worked-fraction of a level's leaves — Task 6's own
/// continuation signal.
fn realized_worked_fraction(level: &Level) -> f64 {
    if level.leaf_styles.is_empty() {
        return NEUTRAL_WORKED_BIAS;
    }
    let worked = level.leaf_styles.iter().filter(|s| s.worked).count();
    worked as f64 / level.leaf_styles.len() as f64
}

/// Generate every rung of one descent under one entrance (spec §4.5).
/// `CaveKind` is fixed for the whole descent (a cave system has one kind —
/// see the spec's §4.5 correction); `origins`/`depths_m` vary per rung,
/// parallel to `rungs`. Each rung's realized worked-fraction becomes the
/// next rung's inherited bias, so a whole descent can read as uniformly
/// natural, uniformly worked, or genuinely transitioning — not
/// independently re-rolled at every rung.
///
/// **Draws a fresh per-rung `Seed` from `UNDERWORLD_LEVEL_DESCENT`, once
/// per rung, rather than passing the same top-level `seed` to every rung's
/// `generate_level_with_water` call.** Every stream a level's own carving
/// derives (`_STYLE`, `_CELLULAR`, `_TUNNELER`, `_ROOMS`) is derived fresh
/// from whatever `Seed` it's given — so two rungs handed the identical
/// `seed` would restart their own carving from identical stream state and
/// produce correlated, near-duplicate shapes, the same defect class
/// `carve`'s per-leaf fix (this file, Task 3/4) exists to prevent, one
/// level up. A drawn `u64` re-wrapped as `Seed(..)` is fully reproducible
/// (still a pure function of the original `seed`) without needing
/// `generate_level_with_origin`'s whole call chain restructured to thread a
/// persistent stream across rungs the way it already does across leaves
/// within one level. `plan` is grown separately, once, from `seed` and its
/// own vertex/rungs — its own streams (`UNDERWORLD_PLAN_*`) are independent
/// of this per-rung carving draw.
/// type-audit: bare-ok(diagnostic-value: depths_m), bare-ok(diagnostic-value: water_table_m)
pub fn generate_descent(
    rungs: &[hornvale_kernel::Band],
    cave_kind: hornvale_terrain::CaveKind,
    origins: &[hornvale_worldgen::chamber::ChamberOrigin],
    depths_m: &[f64],
    water_table_m: f64,
    plan: &hornvale_worldgen::circuit::DescentPlan,
    seed: Seed,
) -> Vec<Level> {
    generate_descent_for_character(
        rungs,
        cave_kind,
        origins,
        depths_m,
        water_table_m,
        hornvale_worldgen::character::Character::WildCave,
        plan,
        seed,
    )
}

/// The initial worked-bias one [`Character`]'s engine set starts its
/// descent with (The Stope, Task 4; spec §A.2/A.3). A character selects
/// among engine SETS by parameterizing the ONE descent engine's
/// worked/natural mix — not by adding three wholly new generators, and not
/// by varying what [`CaveKind`](hornvale_terrain::CaveKind) already varies:
/// kind chooses among the NATURAL-leaf algorithms (Karst→CellularCave,
/// LavaTube→Tunneler, Fracture→AngularRooms), so character moves the
/// ORTHOGONAL axis, how many leaves are worked at all. WildCave sits exactly
/// at [`NEUTRAL_WORKED_BIAS`], which is what keeps `generate_descent`'s
/// historical output byte-identical through the delegation below.
/// type-audit: bare-ok(ratio)
fn engine_worked_bias(character: hornvale_worldgen::character::Character) -> f64 {
    use hornvale_worldgen::character::Character;
    match character {
        Character::WildCave => NEUTRAL_WORKED_BIAS,
        // Gardens are cultivated in living caverns: mildly shaped, mostly
        // found space.
        Character::FungalGardens => 0.35,
        // A drow-tier civilization carves: the descent reads as built.
        Character::DrowTier => 0.85,
    }
}

/// As [`generate_descent`], with `character` selecting the engine set (The
/// Stope, Task 4; spec §A.2/A.3): one descent implementation behind a
/// selector, parameterized rather than tripled. The character's only lever
/// is the INITIAL worked-bias the descent's inertia compounding starts
/// from ([`engine_worked_bias`]) — every seeded draw, stream label and
/// consumption order is exactly `generate_descent`'s, so the WildCave path
/// is byte-identical to it and no stream contract moves.
/// type-audit: bare-ok(diagnostic-value: depths_m), bare-ok(diagnostic-value: water_table_m)
#[allow(clippy::too_many_arguments)]
pub fn generate_descent_for_character(
    rungs: &[hornvale_kernel::Band],
    cave_kind: hornvale_terrain::CaveKind,
    origins: &[hornvale_worldgen::chamber::ChamberOrigin],
    depths_m: &[f64],
    water_table_m: f64,
    character: hornvale_worldgen::character::Character,
    plan: &hornvale_worldgen::circuit::DescentPlan,
    seed: Seed,
) -> Vec<Level> {
    assert_eq!(rungs.len(), origins.len(), "one origin per rung");
    assert_eq!(rungs.len(), depths_m.len(), "one depth per rung");
    assert_eq!(
        plan.rungs.as_slice(),
        rungs,
        "the plan was grown for these rungs"
    );
    let mut descent_stream = seed
        .derive(crate::streams::UNDERWORLD_LEVEL_DESCENT)
        .stream();
    let mut bias = engine_worked_bias(character);
    let mut levels = Vec::with_capacity(rungs.len());
    for (i, &rung) in rungs.iter().enumerate() {
        let extent = generate_level_extent(rung);
        let rung_seed = Seed(descent_stream.next_u64());
        let level = generate_level_with_water(
            extent,
            cave_kind,
            origins[i],
            character,
            depths_m[i],
            water_table_m,
            bias,
            plan,
            i,
            rung_seed,
        );
        bias = realized_worked_fraction(&level);
        levels.push(level);
    }
    levels
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_worldgen::character::Character;
    use std::collections::BTreeMap;

    /// A single-rung plan over `Band::Undercroft` for `kind`, `WildCave` —
    /// the fixture every direct `generate_level_with_origin` test below
    /// threads through since The Crosscut retired the partition tree.
    fn one_rung_plan(
        seed: u64,
        kind: hornvale_terrain::CaveKind,
    ) -> hornvale_worldgen::circuit::DescentPlan {
        hornvale_worldgen::circuit::plan_descent(
            Seed(seed),
            hornvale_kernel::Vertex(0),
            &[hornvale_kernel::Band::Undercroft],
            kind,
            Character::WildCave,
        )
    }

    fn two_rung_plan(seed: u64) -> hornvale_worldgen::circuit::DescentPlan {
        hornvale_worldgen::circuit::plan_descent(
            Seed(seed),
            hornvale_kernel::Vertex(0),
            &[
                hornvale_kernel::Band::Undercroft,
                hornvale_kernel::Band::Shallows,
            ],
            hornvale_terrain::CaveKind::Fracture,
            Character::WildCave,
        )
    }

    /// The movement-mode seam (The Gallery, Task 4; spec §3.2 part 3),
    /// pinned kind by kind: `Wall` is the one impassable kind, `Flooded`
    /// wades rather than refuses (part 1's own rule), and both `Floor` and
    /// a rung connection walk.
    ///
    /// **The Brattice's three kinds are pinned here too** (spec §3.5): a
    /// `Threshold` and a `Drop` are ordinary footing — a squeeze is an
    /// opening in a wall and a chute's lip is a cell you may stand on — and
    /// `Deep` is the one kind that answers `Swim`, which is what makes The
    /// Gallery's reserved variant reachable at last.
    #[test]
    fn movement_mode_answers_one_mode_per_passable_kind() {
        assert_eq!(
            movement_mode(LevelCellKind::Threshold),
            Some(MovementMode::Walk)
        );
        assert_eq!(movement_mode(LevelCellKind::Drop), Some(MovementMode::Walk));
        assert_eq!(movement_mode(LevelCellKind::Deep), Some(MovementMode::Swim));
        assert_eq!(
            movement_mode(LevelCellKind::Floor),
            Some(MovementMode::Walk)
        );
        assert_eq!(
            movement_mode(LevelCellKind::StairsDown),
            Some(MovementMode::Walk)
        );
        assert_eq!(
            movement_mode(LevelCellKind::StairsUp),
            Some(MovementMode::Walk)
        );
        assert_eq!(
            movement_mode(LevelCellKind::Flooded),
            Some(MovementMode::Wade)
        );
        assert_eq!(movement_mode(LevelCellKind::Wall), None);
    }

    #[test]
    fn generation_is_deterministic() {
        let extent = generate_level_extent(hornvale_kernel::Band::Undercroft);
        let a = generate_level(extent, hornvale_kernel::Seed(42));
        let b = generate_level(extent, hornvale_kernel::Seed(42));
        assert_eq!(a, b, "same seed must produce byte-identical levels");
    }

    #[test]
    fn every_cell_of_the_extent_has_a_kind() {
        let extent = generate_level_extent(hornvale_kernel::Band::Undercroft);
        let level = generate_level(extent, hornvale_kernel::Seed(1));
        for x in extent.x..(extent.x + extent.w) {
            for y in extent.y..(extent.y + extent.h) {
                assert!(
                    level.cells.get(Cell(x, y)).is_some(),
                    "cell ({x}, {y}) missing from a total level"
                );
            }
        }
        assert_eq!(
            level.cells.iter().count(),
            (extent.w * extent.h) as usize,
            "no cell outside the extent"
        );
    }

    /// claim: rate(seed: 0..200) — the WORKED-FRACTION across a 200-seed
    /// sweep is compared between `Made` and `Found` origins, a statistical
    /// mean-property claim, not a per-seed-without-exception invariant.
    #[test]
    fn made_chambers_lean_worked_found_chambers_lean_natural() {
        use hornvale_kernel::Band;
        use hornvale_terrain::CaveKind;
        use hornvale_worldgen::chamber::ChamberOrigin;

        let mut made_worked = 0;
        let mut found_worked = 0;
        const TRIALS: u64 = 200;
        let extent = generate_level_extent(Band::Undercroft);
        for s in 0..TRIALS {
            let plan = one_rung_plan(s, CaveKind::Karst);
            let made = generate_level_with_origin(
                extent,
                CaveKind::Karst,
                ChamberOrigin::Made,
                Character::WildCave,
                NEUTRAL_WORKED_BIAS,
                &plan,
                0,
                Seed(s),
            );
            let found = generate_level_with_origin(
                extent,
                CaveKind::Karst,
                ChamberOrigin::Found,
                Character::WildCave,
                NEUTRAL_WORKED_BIAS,
                &plan,
                0,
                Seed(s),
            );
            if made.leaf_styles.iter().any(|s| s.worked) {
                made_worked += 1;
            }
            if found.leaf_styles.iter().any(|s| s.worked) {
                found_worked += 1;
            }
        }
        assert!(
            made_worked > found_worked,
            "made ({made_worked}/{TRIALS}) should read worked more often than found ({found_worked}/{TRIALS})"
        );
    }

    /// claim: rate(seed: single) — not a sweep at all; this asserts a
    /// design invariant (that `ChamberOrigin` is `Copy` and untouched by
    /// the call) via a single representative seed, so the seed-shaped
    /// binding in the caller's own detection is incidental rather than a
    /// statistical claim over a range.
    #[test]
    fn chamber_origin_is_never_mutated_by_geometry() {
        use hornvale_kernel::Band;
        use hornvale_terrain::CaveKind;
        use hornvale_worldgen::chamber::ChamberOrigin;

        let extent = generate_level_extent(Band::Undercroft);
        let plan = one_rung_plan(4, CaveKind::Fracture);
        let origin = ChamberOrigin::Found;
        let _ = generate_level_with_origin(
            extent,
            CaveKind::Fracture,
            origin,
            Character::WildCave,
            NEUTRAL_WORKED_BIAS,
            &plan,
            0,
            Seed(4),
        );
        // `origin` is Copy and untouched by the call above — this test
        // exists to assert the DESIGN, not the runtime: the function takes
        // `origin` by value and returns nothing that could feed back into
        // it, so ChamberOrigin's own ecology contract (is_sump's dryness
        // rule, spec §4.3) cannot be perturbed by this module. Compiling is
        // the proof.
        assert_eq!(origin, ChamberOrigin::Found);
    }

    /// The Gallery, Task 4: superseded `a_sump_gets_a_flooded_region_a_made_
    /// chamber_never_does`, whose "a Made chamber is drained regardless of
    /// the water table" assertion encoded the OLD rule — flooding gated on
    /// `origin` through `is_sump` — that spec §3.2's rewrite retires.
    /// Wetness now keys on each leaf's own `LeafStyle.worked`, so this
    /// asserts the new invariant directly against the leaves themselves
    /// rather than against `origin`: under a phreatic column, a worked leaf
    /// stays fully drained and at least one natural leaf floods; above the
    /// water table, nothing floods regardless of the worked/natural mix.
    ///
    /// **`Seed(4)` is load-bearing, not arbitrary**, re-verified against the
    /// plan-grid regions The Crosscut introduced: it grows into more than
    /// one region, at least one of each worked/natural kind, so both
    /// `if`/`else` arms of the loop below actually execute — `any_worked_leaf`
    /// and `any_natural_leaf_flooded` below assert that they did, so a future
    /// regression back to a single-region-only fixture fails loudly here
    /// rather than silently passing again.
    #[test]
    fn a_phreatic_level_floods_its_natural_leaves_and_drains_its_worked_ones() {
        use hornvale_kernel::Band;
        use hornvale_terrain::CaveKind;
        use hornvale_worldgen::chamber::ChamberOrigin;

        let extent = generate_level_extent(Band::Undercroft);
        let plan = one_rung_plan(4, CaveKind::Karst);
        // depth_m > water_table_m => phreatic (is_phreatic's own contract).
        let sump = generate_level_with_water(
            extent,
            CaveKind::Karst,
            ChamberOrigin::Found,
            Character::WildCave,
            100.0,
            10.0,
            NEUTRAL_WORKED_BIAS,
            &plan,
            0,
            Seed(4),
        );
        let node_ids = plan.nodes_on(0);
        assert_eq!(
            node_ids.len(),
            sump.leaf_styles.len(),
            "nodes_on(level) and leaf_styles must stay index-aligned"
        );
        let mut any_worked_leaf = false;
        let mut any_natural_leaf_flooded = false;
        for (&id, style) in node_ids.iter().zip(sump.leaf_styles.iter()) {
            let rect = rect_of(&plan, id);
            let leaf_has_flood = (rect.x..(rect.x + rect.w)).any(|x| {
                (rect.y..(rect.y + rect.h))
                    .any(|y| sump.cells.get(Cell(x, y)) == Some(LevelCellKind::Flooded))
            });
            if style.worked {
                any_worked_leaf = true;
                assert!(
                    !leaf_has_flood,
                    "a worked leaf must stay drained even under a phreatic water table"
                );
            } else if leaf_has_flood {
                any_natural_leaf_flooded = true;
            }
        }
        assert!(
            any_worked_leaf,
            "the fixture must carve at least one worked leaf, or the \
             worked-stays-drained branch above never ran"
        );
        assert!(
            any_natural_leaf_flooded,
            "a phreatic Found chamber must carve at least one flooded natural leaf"
        );

        let dry = generate_level_with_water(
            extent,
            CaveKind::Karst,
            ChamberOrigin::Found,
            Character::WildCave,
            5.0,
            10.0,
            NEUTRAL_WORKED_BIAS,
            &plan,
            0,
            Seed(4),
        );
        assert!(
            dry.cells.iter().all(|(_, k)| k != LevelCellKind::Flooded),
            "a vadose Found chamber (above the water table) stays dry"
        );
    }

    /// spec §3.2 part 2's own regression: wetness keys on `LeafStyle.worked`,
    /// so a heavily-worked character's descent must come out substantially
    /// drier than a natural-cave character's, on the SAME seed and the SAME
    /// depth/water-table inputs — the only thing that differs between the
    /// two runs is how many leaves read as worked. Asserts the DIRECTION
    /// and a MARGIN, not a fixed percentage: the percentage is a
    /// calibration this task does not own.
    ///
    /// **Restated as a RATIO under The Crosscut's realizer, and the
    /// mechanism corrected.** `flooded_fraction` divides by every cell of
    /// the extent, not just the walkable ones — a passage/stair corridor's
    /// never-flooding floor moves neither numerator nor denominator of
    /// THAT fraction, so it is not what shrank the old 0.05 absolute
    /// margin. The real driver: the plan's region rects (their grid pitch
    /// is `circuit::REGION_SPAN`, with a one-cell wall on every side) cover
    /// a smaller share of the extent than the old BSP leaves did, so both
    /// characters' flooded fractions are smaller now, in absolute terms,
    /// than they were under the retired realizer — an absolute margin
    /// tuned against the old geometry does not survive a new one that
    /// simply carves less floor overall. A RATIO does survive it: it
    /// compares drow's wetness to wild's wetness relative to each other,
    /// not to a fixed absolute budget of flooded cells, so it is
    /// insensitive to how much of the extent the realizer carves at all —
    /// only to whether drow reads reliably drier than wild, which is the
    /// actual claim. Measured directly against this realizer: a 200-seed
    /// sweep gave drow=0.039, wild=0.069 (ratio 0.039/0.069 ≈ 0.565), and
    /// the 30-seed sweep this test actually runs gives the same ratio
    /// (≈0.565). `0.7` stays comfortably above the measured ≈0.565 while
    /// still asserting the same direction with room for seed-to-seed
    /// noise, and needs no future retune if a later campaign changes how
    /// much of the extent gets carved — only if the RELATIVE dryness
    /// between characters changes.
    ///
    /// claim: rate(seed: 0..30) — a mean-flooded-fraction RATIO comparison
    /// across a 30-seed sweep, not a per-seed-without-exception invariant.
    #[test]
    fn a_drow_tier_descent_comes_out_substantially_drier_than_a_wild_cave_one() {
        use hornvale_kernel::Band;
        use hornvale_terrain::CaveKind;
        use hornvale_worldgen::chamber::ChamberOrigin;

        let rungs = [Band::Undercroft, Band::Shallows, Band::Deeps];
        let origins = [ChamberOrigin::Found; 3];
        // Every rung phreatic (a deep column against a shallow table), so
        // any dryness difference between the two runs comes only from the
        // worked/natural mix a character selects, not from whether the
        // column is wet at all.
        let depths_m = [500.0, 500.0, 500.0];
        let water_table_m = 10.0;

        fn flooded_fraction(levels: &[Level]) -> f64 {
            let total: usize = levels.iter().map(|l| l.cells.iter().count()).sum();
            let flooded: usize = levels
                .iter()
                .map(|l| {
                    l.cells
                        .iter()
                        .filter(|(_, k)| *k == LevelCellKind::Flooded)
                        .count()
                })
                .sum();
            flooded as f64 / total.max(1) as f64
        }

        const TRIALS: u64 = 30;
        let mut drow_total = 0.0;
        let mut wild_total = 0.0;
        for s in 0..TRIALS {
            let drow_plan = hornvale_worldgen::circuit::plan_descent(
                Seed(s),
                hornvale_kernel::Vertex(0),
                &rungs,
                CaveKind::Karst,
                Character::DrowTier,
            );
            let wild_plan = hornvale_worldgen::circuit::plan_descent(
                Seed(s),
                hornvale_kernel::Vertex(0),
                &rungs,
                CaveKind::Karst,
                Character::WildCave,
            );
            let drow = generate_descent_for_character(
                &rungs,
                CaveKind::Karst,
                &origins,
                &depths_m,
                water_table_m,
                Character::DrowTier,
                &drow_plan,
                Seed(s),
            );
            let wild = generate_descent_for_character(
                &rungs,
                CaveKind::Karst,
                &origins,
                &depths_m,
                water_table_m,
                Character::WildCave,
                &wild_plan,
                Seed(s),
            );
            drow_total += flooded_fraction(&drow);
            wild_total += flooded_fraction(&wild);
        }
        let drow_mean = drow_total / TRIALS as f64;
        let wild_mean = wild_total / TRIALS as f64;
        assert!(
            drow_mean < 0.7 * wild_mean,
            "a drow-tier descent must come out substantially drier than a \
             wild-cave one, as a RATIO of the two means (not an absolute \
             margin — see this test's own doc): drow={drow_mean:.3}, \
             wild={wild_mean:.3}, ratio={:.3}",
            drow_mean / wild_mean
        );
    }

    /// claim: rate(seed: 0..100) — the deepest rung's mean worked-fraction
    /// across a 100-seed sweep is compared against the neutral baseline, a
    /// statistical mean-property claim (compounding inertia), not a
    /// per-seed-without-exception invariant.
    #[test]
    fn worked_fraction_has_inertia_across_rungs() {
        use hornvale_kernel::Band;
        use hornvale_terrain::CaveKind;
        use hornvale_worldgen::chamber::ChamberOrigin;

        let rungs = [Band::Undercroft, Band::Shallows, Band::Deeps];
        // All three rungs Made: with inertia, later rungs' worked fraction
        // should not regress toward NEUTRAL_WORKED_BIAS as hard as an
        // independent re-roll would — measured as "the deepest rung's worked
        // fraction, averaged over many seeds, exceeds the neutral baseline".
        const TRIALS: u64 = 100;
        let mut deepest_worked_total = 0.0;
        for s in 0..TRIALS {
            let origins = [
                ChamberOrigin::Made,
                ChamberOrigin::Made,
                ChamberOrigin::Made,
            ];
            let depths_m = [20.0, 60.0, 120.0];
            let plan = hornvale_worldgen::circuit::plan_descent(
                Seed(s),
                hornvale_kernel::Vertex(0),
                &rungs,
                CaveKind::Karst,
                Character::WildCave,
            );
            let levels = generate_descent(
                &rungs,
                CaveKind::Karst,
                &origins,
                &depths_m,
                500.0,
                &plan,
                Seed(s),
            );
            let deepest = levels.last().expect("three rungs requested");
            let worked = deepest.leaf_styles.iter().filter(|s| s.worked).count() as f64;
            let total = deepest.leaf_styles.len().max(1) as f64;
            deepest_worked_total += worked / total;
        }
        let mean_worked_fraction = deepest_worked_total / TRIALS as f64;
        assert!(
            mean_worked_fraction > NEUTRAL_WORKED_BIAS,
            "three Made rungs should compound toward worked, not float at neutral: got {mean_worked_fraction}"
        );
    }

    /// claim: rate(seed: single) — asserts a level-count invariant at one
    /// representative seed, not a statistical claim over a range.
    #[test]
    fn generate_descent_produces_one_level_per_rung() {
        use hornvale_kernel::Band;
        use hornvale_terrain::CaveKind;
        use hornvale_worldgen::chamber::ChamberOrigin;

        let rungs = [Band::Undercroft, Band::Shallows];
        let origins = [ChamberOrigin::Found, ChamberOrigin::Found];
        let depths_m = [20.0, 60.0];
        let plan = hornvale_worldgen::circuit::plan_descent(
            Seed(1),
            hornvale_kernel::Vertex(0),
            &rungs,
            CaveKind::LavaTube,
            Character::WildCave,
        );
        let levels = generate_descent(
            &rungs,
            CaveKind::LavaTube,
            &origins,
            &depths_m,
            500.0,
            &plan,
            Seed(1),
        );
        assert_eq!(levels.len(), 2);
    }

    /// A plan over the REAL habitation ladder (`hornvale_terrain::rungs()`
    /// minus `Surface`, five rungs today) — the object `Underground::enter`
    /// actually walks. The two-rung stub below it cannot see the defect the
    /// stairs-pairing test exists for: a coordinate collision needs a
    /// MIDDLE rung, where one node is the upper end of one stairway and the
    /// lower end of another.
    fn habitation_ladder() -> Vec<hornvale_kernel::Band> {
        hornvale_terrain::rungs()
            .iter()
            .copied()
            .filter(|r| *r != hornvale_kernel::Band::Surface)
            .collect()
    }

    fn full_ladder_plan(seed: u64) -> hornvale_worldgen::circuit::DescentPlan {
        hornvale_worldgen::circuit::plan_descent(
            Seed(seed),
            hornvale_kernel::Vertex(0),
            &habitation_ladder(),
            hornvale_terrain::CaveKind::Fracture,
            Character::WildCave,
        )
    }

    /// claim: invariant(seed: 0..200) — THE CROSSCUT's stairs contract
    /// (spec §3.3): every `StairsDown` on rung `i` (below the last)
    /// has a `StairsUp` at the SAME coordinate on rung `i+1` and vice versa;
    /// rung 0 has no `StairsUp`; the last rung has exactly one `StairsDown`,
    /// the dangling terminus. Replaces
    /// `stairs_down_and_stairs_up_never_share_a_cell`, whose "exactly one"
    /// the plan deliberately breaks.
    ///
    /// **Amended by The Brattice (spec §3.5), not replaced**: a chute writes
    /// `Drop` on rung `i` and NO `StairsUp` on rung `i+1`, so the pairing
    /// above is about the stairs that remain; every `Drop` is additionally
    /// checked to sit over a standable non-stair cell.
    ///
    /// **Swept over the FULL habitation ladder since the final review**, not
    /// a two-rung stub: a stair-coordinate collision requires a middle rung
    /// (a node that is the upper end of one stairway and the lower end of
    /// another), so the two-rung version could not observe the defect at
    /// all — the realizer's `stairs_into` loop overwrote a `StairsDown` the
    /// `stairs_from` loop had written, leaving an orphan `StairsUp` below.
    #[test]
    fn stairs_pair_by_coordinate_across_adjacent_rungs() {
        use hornvale_terrain::CaveKind;
        use hornvale_worldgen::chamber::ChamberOrigin;
        let rungs = habitation_ladder();
        let origins = vec![ChamberOrigin::Found; rungs.len()];
        let depths_m = [20.0, 60.0, 120.0, 250.0, 500.0];
        assert_eq!(
            depths_m.len(),
            rungs.len(),
            "the depth ladder must have one entry per habitation rung"
        );
        let mut paired_stairs_seen = 0usize;
        for s in 0..200u64 {
            let plan = full_ladder_plan(s);
            let levels = generate_descent(
                &rungs,
                CaveKind::Fracture,
                &origins,
                &depths_m,
                500.0,
                &plan,
                Seed(s),
            );
            let cells_of = |l: &Level, k: LevelCellKind| -> Vec<Cell> {
                l.cells
                    .iter()
                    .filter(|(_, kk)| *kk == k)
                    .map(|(c, _)| c)
                    .collect()
            };
            assert!(
                cells_of(&levels[0], LevelCellKind::StairsUp).is_empty(),
                "seed {s}: rung 0 has stairs up"
            );
            for i in 0..levels.len() - 1 {
                let downs = cells_of(&levels[i], LevelCellKind::StairsDown);
                let ups = cells_of(&levels[i + 1], LevelCellKind::StairsUp);
                assert_eq!(
                    downs,
                    ups,
                    "seed {s}: stairs down on rung {i} must equal stairs up on rung {} by coordinate",
                    i + 1
                );
                assert!(!downs.is_empty(), "seed {s}: no stairway off rung {i}");
                paired_stairs_seen += downs.len();
                // The Brattice, spec §3.5: a chute's lip pairs with a
                // standable NON-stair cell one rung down — never a
                // `StairsUp`, which is the whole asymmetry (`down` takes a
                // chute; `up` from beneath it needs `Fly`).
                for d in cells_of(&levels[i], LevelCellKind::Drop) {
                    let below = levels[i + 1].cells.get(d);
                    assert!(
                        matches!(below, Some(k) if movement_mode(k).is_some()),
                        "seed {s}: a Drop on rung {i} at {d:?} has {below:?} beneath it"
                    );
                    assert!(
                        !matches!(
                            below,
                            Some(LevelCellKind::StairsUp) | Some(LevelCellKind::StairsDown)
                        ),
                        "seed {s}: a Drop on rung {i} at {d:?} sits over a stair"
                    );
                }
            }
            let last = levels.len() - 1;
            let downs_last = cells_of(&levels[last], LevelCellKind::StairsDown);
            assert_eq!(
                downs_last.len(),
                1,
                "seed {s}: the last rung carries exactly the dangling terminus: {downs_last:?}"
            );
        }
        assert!(
            paired_stairs_seen > 200,
            "the sweep must exercise multi-stair rungs, not one stair each"
        );
    }

    /// claim: invariant(seed: 0..200 x 3 (kind, character) pairs) — THE
    /// BRATTICE's realization witness (spec §3.5). For every plan: every
    /// `Passage` edge realizes exactly ONE crossing cell on the divider
    /// between its two regions, recorded in [`Level::thresholds`]; that cell
    /// is a `Threshold`, or a `Deep` when the edge is a sump; the count of
    /// `Threshold` cells on a level equals the count of its non-sump
    /// passages, so nothing of the kind exists that no edge asked for; every
    /// `Needs(Mode(Fly))` stair is a `Drop` above a standable non-stair
    /// cell; every other stair still pairs `StairsDown` with `StairsUp`; and
    /// the count of `Drop` cells equals the count of chutes. Both
    /// directions, which is the point: the walk reads the realization and
    /// never the plan (spec §3.6), so a solvability proof about the plan is
    /// a proof about the walked level only while this test is green.
    ///
    /// **A sump's crossing cell is `Deep`, not `Threshold`** — an execution
    /// amendment to spec §3.5, whose prose read "every passage has one
    /// `Threshold`". One cell has one kind, and the crossing of a sump whose
    /// L-corridor is a single cell would otherwise realize no `Deep` at all.
    /// `Level::thresholds` still records a crossing for EVERY passage, sump
    /// or not, because that is the place a door Thing anchors (§3.7).
    ///
    /// Positive controls: the sweep must actually observe at least one sump
    /// and one chute, or every gate-conditional branch above would pass
    /// vacuously against a plan that stamped nothing.
    #[test]
    fn the_realization_witnesses_exactly_what_the_plan_stamped() {
        use hornvale_terrain::CaveKind;
        use hornvale_worldgen::brattice::{Capability, Requirement, Way};
        use hornvale_worldgen::chamber::ChamberOrigin;
        use std::collections::{BTreeSet, VecDeque};

        let rungs = habitation_ladder();
        let origins = vec![ChamberOrigin::Found; rungs.len()];
        let depths_m = vec![10.0; rungs.len()];
        let mut feet_walked = 0usize;
        let mut sumps_seen = 0usize;
        let mut chutes_seen = 0usize;
        let mut passages_seen = 0usize;
        for seed in 0..200u64 {
            for (kind, ch) in [
                (CaveKind::Karst, Character::DrowTier),
                (CaveKind::Fracture, Character::WildCave),
                (CaveKind::LavaTube, Character::WildCave),
            ] {
                let plan = hornvale_worldgen::circuit::plan_descent(
                    Seed(seed),
                    hornvale_kernel::Vertex(1),
                    &rungs,
                    kind,
                    ch,
                );
                let levels = generate_descent_for_character(
                    &rungs,
                    kind,
                    &origins,
                    &depths_m,
                    1000.0,
                    ch,
                    &plan,
                    Seed(seed),
                );
                let is_sump = |a: usize, b: usize| {
                    matches!(
                        plan.gate_between(a, b).map(|(_, g)| g.toward_a),
                        Some(Way::Needs(Requirement::Mode(Capability::Swim)))
                    )
                };
                let is_chute = |a: usize, b: usize| {
                    matches!(
                        plan.gate_between(a, b).map(|(_, g)| g.toward_a),
                        Some(Way::Needs(Requirement::Mode(Capability::Fly)))
                    )
                };
                for (l, level) in levels.iter().enumerate() {
                    // (a) every passage realizes exactly one crossing cell,
                    // of the kind its gate asks for.
                    for (a, b) in plan.passages_on(l) {
                        passages_seen += 1;
                        let crossings: Vec<Cell> = level
                            .thresholds
                            .iter()
                            .filter(|(x, y, _)| (*x == a && *y == b) || (*x == b && *y == a))
                            .map(|t| t.2)
                            .collect();
                        assert_eq!(
                            crossings.len(),
                            1,
                            "seed {seed} {kind:?}/{ch:?} level {l} edge {a}-{b}: \
                             {} crossings recorded",
                            crossings.len()
                        );
                        let want = if is_sump(a, b) {
                            sumps_seen += 1;
                            LevelCellKind::Deep
                        } else {
                            LevelCellKind::Threshold
                        };
                        assert_eq!(
                            level.cells.get(crossings[0]),
                            Some(want),
                            "seed {seed} {kind:?}/{ch:?} level {l} edge {a}-{b}: \
                             the crossing at {:?} is not {want:?}",
                            crossings[0]
                        );
                    }
                    // (b) no Threshold exists that no passage asked for, and
                    // none is missing.
                    let non_sump = plan
                        .passages_on(l)
                        .iter()
                        .filter(|(a, b)| !is_sump(*a, *b))
                        .count();
                    let thresholds = level
                        .cells
                        .iter()
                        .filter(|(_, k)| *k == LevelCellKind::Threshold)
                        .count();
                    assert_eq!(
                        thresholds, non_sump,
                        "seed {seed} {kind:?}/{ch:?} level {l}: a Threshold nobody \
                         asked for, or one missing"
                    );
                    // (c) a chute is a Drop over a standable non-stair cell;
                    // every other stairway still pairs.
                    for (upper, lower, x, y) in plan.stairs_from(l) {
                        let here = level.cells.get(Cell(x, y));
                        let below = levels[l + 1].cells.get(Cell(x, y));
                        if is_chute(upper, lower) {
                            chutes_seen += 1;
                            assert_eq!(
                                here,
                                Some(LevelCellKind::Drop),
                                "seed {seed} {kind:?}/{ch:?} level {l}: chute lip at \
                                 ({x}, {y}) is {here:?}"
                            );
                            // `Floor | Flooded` exactly: the landing is
                            // written `Floor` by `place_stair` AFTER the
                            // passage loop has stamped every crossing, so a
                            // `Threshold` landing is not merely rare, it is
                            // unreachable — and admitting a kind the code
                            // cannot produce is an assertion that cannot
                            // fail for a case that cannot happen.
                            assert!(
                                matches!(
                                    below,
                                    Some(LevelCellKind::Floor) | Some(LevelCellKind::Flooded)
                                ),
                                "seed {seed} {kind:?}/{ch:?} level {l}: a chute lands \
                                 on {below:?}"
                            );
                        } else {
                            assert_eq!(
                                here,
                                Some(LevelCellKind::StairsDown),
                                "seed {seed} {kind:?}/{ch:?} level {l}: stair head at \
                                 ({x}, {y}) is {here:?}"
                            );
                            assert_eq!(
                                below,
                                Some(LevelCellKind::StairsUp),
                                "seed {seed} {kind:?}/{ch:?} level {l}: stair foot at \
                                 ({x}, {y}) is {below:?}"
                            );
                        }
                    }
                    let drops = level
                        .cells
                        .iter()
                        .filter(|(_, k)| *k == LevelCellKind::Drop)
                        .count();
                    let chutes = plan
                        .stairs_from(l)
                        .iter()
                        .filter(|(u, lo, _, _)| is_chute(*u, *lo))
                        .count();
                    assert_eq!(
                        drops, chutes,
                        "seed {seed} {kind:?}/{ch:?} level {l}: a Drop nobody asked for"
                    );
                    // (d) NO UNSTAMPED SWIM (Ruling G's cheap half): a level
                    // none of whose passages is a sump has no `Deep` cell at
                    // all. Deep water is a gate the plan stamps; a `Deep`
                    // cell on a level with no sump is a swim requirement
                    // nobody asked for, and unlike (b) this arm costs one
                    // pass over the grid.
                    if plan.passages_on(l).iter().all(|(a, b)| !is_sump(*a, *b)) {
                        let deeps = level
                            .cells
                            .iter()
                            .filter(|(_, k)| *k == LevelCellKind::Deep)
                            .count();
                        assert_eq!(
                            deeps, 0,
                            "seed {seed} {kind:?}/{ch:?} level {l}: {deeps} Deep cells on a \
                             level with no sump"
                        );
                    }
                    // (e) THE WALKER ARM (Ruling G): every way a body meets
                    // vertically — a stair head, a stair foot, a chute's lip,
                    // a chute's landing — is reachable ON FOOT from ordinary
                    // footing of its own region. `Walk`/`Wade` only, never
                    // `Swim`: both connectivity sweeps ask
                    // `movement_mode(..).is_some()`, which `Deep` satisfies,
                    // so a stair standing behind deep water would be
                    // "connected" to both of them and unreachable to every
                    // body the plan gave the capability to. Confined to the
                    // cell's own region rect, because that is the scope
                    // `place_stair`'s own foot connector and repair work in.
                    let regions: Vec<Rect> = (0..plan.nodes.len())
                        .filter(|n| plan.nodes[*n].level as usize == l)
                        .map(|n| rect_of(&plan, n))
                        .collect();
                    let mut feet: Vec<Cell> = level
                        .cells
                        .iter()
                        .filter(|(_, k)| {
                            matches!(
                                k,
                                LevelCellKind::StairsDown
                                    | LevelCellKind::StairsUp
                                    | LevelCellKind::Drop
                            )
                        })
                        .map(|(c, _)| c)
                        .collect();
                    for (upper, lower, x, y) in plan.stairs_into(l) {
                        if is_chute(upper, lower) {
                            feet.push(Cell(x, y));
                        }
                    }
                    for foot in feet {
                        let Some(&rect) = regions.iter().find(|r| r.contains(foot)) else {
                            continue; // the terminus stair sits in its own region; if no
                            // region on this level claims the cell there is no
                            // rect to confine a walk to.
                        };
                        let on_foot = |c: Cell| {
                            rect.contains(c)
                                && matches!(
                                    level.cells.get(c).and_then(movement_mode),
                                    Some(MovementMode::Walk) | Some(MovementMode::Wade)
                                )
                        };
                        let mut seen = BTreeSet::new();
                        let mut queue = VecDeque::new();
                        seen.insert(foot);
                        queue.push_back(foot);
                        let mut found_footing = false;
                        while let Some(Cell(x, y)) = queue.pop_front() {
                            if matches!(
                                level.cells.get(Cell(x, y)),
                                Some(LevelCellKind::Floor) | Some(LevelCellKind::Flooded)
                            ) {
                                found_footing = true;
                                break;
                            }
                            for (dx, dy) in [(1, 0), (-1, 0), (0, 1), (0, -1)] {
                                let next = Cell(x + dx, y + dy);
                                if on_foot(next) && seen.insert(next) {
                                    queue.push_back(next);
                                }
                            }
                        }
                        assert!(
                            found_footing,
                            "seed {seed} {kind:?}/{ch:?} level {l}: the way at {foot:?} \
                             ({:?}) reaches no Floor/Flooded cell of its own region \
                             {rect:?} without swimming",
                            level.cells.get(foot)
                        );
                        feet_walked += 1;
                    }
                }
            }
        }
        assert!(passages_seen > 0, "the sweep saw no passage at all");
        assert!(
            feet_walked > 0,
            "the sweep walked away from no stair, lip or landing — the walker arm \
             passed vacuously"
        );
        assert!(
            sumps_seen > 0,
            "the sweep observed no sump — every Deep branch above passed vacuously"
        );
        assert!(
            chutes_seen > 0,
            "the sweep observed no chute — every Drop branch above passed vacuously"
        );
    }

    /// claim: invariant(seed: 0..200) — spec §3.3's other half, previously
    /// unpinned: two grid-adjacent regions with NO `Passage` edge between
    /// them keep a solid wall — never a way through, not even by accident
    /// through a stair-placement repair. Restricts the flood-fill to
    /// exactly the union of the two regions' own rects plus the one-cell
    /// divider between them (the bounding box of the two rects always
    /// covers exactly that divider and nothing beyond it, by construction
    /// of `region_rect`'s even tiling) — never the whole level, where an
    /// unrelated corridor elsewhere could give a false "connected" reading
    /// that has nothing to do with this pair. Counts a cell reachable if
    /// standable (`Floor`/`Flooded`/`StairsDown`/`StairsUp`), matching what
    /// a body can actually cross. Includes a positive control: the sweep
    /// must actually see at least one unlinked adjacent pair, or the whole
    /// test would pass vacuously.
    #[test]
    fn unlinked_neighbours_keep_their_wall() {
        use hornvale_kernel::Band;
        use hornvale_terrain::CaveKind;
        use hornvale_worldgen::chamber::ChamberOrigin;
        use std::collections::{BTreeSet, VecDeque};

        let rungs = [Band::Undercroft, Band::Shallows];
        let origins = [ChamberOrigin::Found, ChamberOrigin::Found];
        let depths_m = [20.0, 60.0];
        let mut unlinked_pairs_seen = 0usize;
        for s in 0..200u64 {
            let plan = two_rung_plan(s);
            let levels = generate_descent(
                &rungs,
                CaveKind::Fracture,
                &origins,
                &depths_m,
                500.0,
                &plan,
                Seed(s),
            );
            for (level_idx, level) in levels.iter().enumerate() {
                let node_ids = plan.nodes_on(level_idx);
                let passages: BTreeSet<(usize, usize)> = plan
                    .passages_on(level_idx)
                    .into_iter()
                    .flat_map(|(a, b)| [(a, b), (b, a)])
                    .collect();
                for &a in &node_ids {
                    for &b in &node_ids {
                        if a >= b {
                            continue;
                        }
                        let ca = plan.nodes[a].cell;
                        let cb = plan.nodes[b].cell;
                        let grid_adjacent = (i32::from(ca.col) - i32::from(cb.col)).abs()
                            + (i32::from(ca.row) - i32::from(cb.row)).abs()
                            == 1;
                        if !grid_adjacent || passages.contains(&(a, b)) {
                            continue;
                        }
                        unlinked_pairs_seen += 1;
                        let ra = rect_of(&plan, a);
                        let rb = rect_of(&plan, b);
                        let union_x0 = ra.x.min(rb.x);
                        let union_y0 = ra.y.min(rb.y);
                        let union_x1 = (ra.x + ra.w).max(rb.x + rb.w);
                        let union_y1 = (ra.y + ra.h).max(rb.y + rb.h);
                        let standable: BTreeSet<Cell> = (union_x0..union_x1)
                            .flat_map(|x| (union_y0..union_y1).map(move |y| Cell(x, y)))
                            .filter(|&c| {
                                matches!(
                                    level.cells.get(c),
                                    Some(LevelCellKind::Floor)
                                        | Some(LevelCellKind::Flooded)
                                        | Some(LevelCellKind::StairsDown)
                                        | Some(LevelCellKind::StairsUp)
                                )
                            })
                            .collect();
                        let starts: Vec<Cell> = standable
                            .iter()
                            .copied()
                            .filter(|c| ra.contains(*c))
                            .collect();
                        let mut seen: BTreeSet<Cell> = BTreeSet::new();
                        let mut queue: VecDeque<Cell> = VecDeque::new();
                        for &c in &starts {
                            if seen.insert(c) {
                                queue.push_back(c);
                            }
                        }
                        while let Some(Cell(x, y)) = queue.pop_front() {
                            for (dx, dy) in [(1, 0), (-1, 0), (0, 1), (0, -1)] {
                                let next = Cell(x + dx, y + dy);
                                if standable.contains(&next) && seen.insert(next) {
                                    queue.push_back(next);
                                }
                            }
                        }
                        assert!(
                            !seen.iter().any(|c| rb.contains(*c)),
                            "seed {s} level {level_idx}: unlinked grid-adjacent nodes {a}/{b} \
                             have a walkable route between them — spec §3.3's wall was crossed"
                        );
                    }
                }
            }
        }
        assert!(
            unlinked_pairs_seen > 0,
            "the sweep never saw an unlinked grid-adjacent pair — widen the seed \
             range or rung count so this test is not vacuous"
        );
    }

    /// claim: rate(seed: single) — asserts a determinism-vs-correlation
    /// invariant at one representative seed, not a statistical claim over a
    /// range.
    #[test]
    fn two_rungs_with_the_same_origin_and_kind_still_differ() {
        // The rung-level twin of carve.rs's "two leaves sharing a stream must
        // differ" regression. Repeats the SAME rung twice — an artificial but
        // precise probe: identical extent, origin, kind and depth for both
        // entries, so the only thing that could make the two produced levels
        // differ is which seed each one draws. Using two DIFFERENT rungs here
        // would be a weaker test: different rungs get different extents
        // (`generate_level_extent`), so their `cells` maps would have
        // different key sets and always compare unequal regardless of whether
        // the seed-reuse defect this test guards against is present.
        use hornvale_kernel::Band;
        use hornvale_terrain::CaveKind;
        use hornvale_worldgen::chamber::ChamberOrigin;

        let rungs = [Band::Undercroft, Band::Undercroft];
        let origins = [ChamberOrigin::Found, ChamberOrigin::Found];
        let depths_m = [20.0, 20.0];
        let plan = hornvale_worldgen::circuit::plan_descent(
            Seed(13),
            hornvale_kernel::Vertex(0),
            &rungs,
            CaveKind::Karst,
            Character::WildCave,
        );
        let levels = generate_descent(
            &rungs,
            CaveKind::Karst,
            &origins,
            &depths_m,
            500.0,
            &plan,
            Seed(13),
        );
        assert_ne!(
            levels[0].cells, levels[1].cells,
            "two rungs must not produce the same cells even with matching origin/kind/depth"
        );
    }

    /// claim: rate(seed: 0..60) — Task 4 (spec §A.2/A.3): two branches with
    /// DIFFERENT characters, over the SAME bands, produce structurally
    /// different descents. Asserted as properties, never a golden string:
    /// (1) the mean realized worked-fraction of the first rung orders by
    /// character — DrowTier (a carving civilization) above WildCave above
    /// FungalGardens — because a character selects an engine set whose dial
    /// positions are the worked/natural mix, the one axis `cave_kind` does
    /// NOT already vary (kind picks among NATURAL-leaf algorithms);
    /// (2) for the majority of individual seeds the two descents' cells
    /// differ somewhere in the descent, so the ordering is not carried by a
    /// statistical shadow alone.
    #[test]
    fn different_characters_produce_structurally_different_descents() {
        use hornvale_kernel::Band;
        use hornvale_terrain::CaveKind;
        use hornvale_worldgen::chamber::ChamberOrigin;

        let rungs = [Band::Undercroft, Band::Shallows];
        let origins = [ChamberOrigin::Found; 2];
        let depths_m = [20.0, 60.0];
        const TRIALS: usize = 60;
        let mut totals = BTreeMap::from([
            (Character::WildCave, 0.0),
            (Character::FungalGardens, 0.0),
            (Character::DrowTier, 0.0),
        ]);
        let mut cells_differ = 0usize;
        for s in 0..TRIALS as u64 {
            let mut cell_sets = BTreeMap::new();
            for character in hornvale_worldgen::character::CHARACTERS {
                let plan = hornvale_worldgen::circuit::plan_descent(
                    Seed(s),
                    hornvale_kernel::Vertex(0),
                    &rungs,
                    CaveKind::Karst,
                    *character,
                );
                let levels = generate_descent_for_character(
                    &rungs,
                    CaveKind::Karst,
                    &origins,
                    &depths_m,
                    500.0,
                    *character,
                    &plan,
                    Seed(s),
                );
                let first = &levels[0];
                let worked = first.leaf_styles.iter().filter(|s| s.worked).count() as f64;
                let total = first.leaf_styles.len().max(1) as f64;
                *totals.get_mut(character).expect("roster character") += worked / total;
                cell_sets.insert(
                    *character,
                    levels.iter().map(|l| l.cells.clone()).collect::<Vec<_>>(),
                );
            }
            if cell_sets[&Character::DrowTier] != cell_sets[&Character::WildCave]
                || cell_sets[&Character::FungalGardens] != cell_sets[&Character::WildCave]
            {
                cells_differ += 1;
            }
        }
        let mean = |c: Character| totals[&c] / TRIALS as f64;
        assert!(
            mean(Character::DrowTier) > mean(Character::WildCave),
            "a drow-tier descent must read more worked than a wild-cave one \
             ({:.3} vs {:.3})",
            mean(Character::DrowTier),
            mean(Character::WildCave)
        );
        assert!(
            mean(Character::FungalGardens) < mean(Character::WildCave),
            "a fungal-gardens descent must read less worked than a wild-cave one \
             ({:.3} vs {:.3})",
            mean(Character::FungalGardens),
            mean(Character::WildCave)
        );
        assert!(
            cells_differ * 2 > TRIALS,
            "same-seed descents under different characters must differ \
             structurally for the majority of seeds: got {cells_differ}/{TRIALS}"
        );
    }

    /// claim: invariant(seed: 0..12 x all characters x all kinds) — Task 4
    /// step 4, reframed for The Crosscut: the connectivity invariant holds
    /// for EVERY engine the selector can pick, i.e. for every character —
    /// but the invariant itself is now over the WHOLE DESCENT, not one
    /// level in isolation.
    ///
    /// **Why the scope widened.** The plan's own graph connectivity
    /// (`every_node_is_reachable_from_the_entrance`,
    /// `windows/worldgen/src/circuit.rs`) is proven across ALL edges —
    /// same-level passages AND cross-level stairs together — never
    /// per-level. A `cycle` (`Realm`) is free to close through a
    /// neighbouring rung, so a region can end up with no SAME-LEVEL
    /// passage at all, reachable only by taking its stair to the level
    /// above or below and back — observed directly for a real descent
    /// (`WildCave`/`Karst` seed 0: three level-1 nodes shared no passage
    /// with the other eleven, connected only through stairs to level 0 and
    /// level 2). Asserting per-level connectivity would fail on exactly
    /// this correct behaviour, not a defect. Flood-fills every level's
    /// standable cells AND crosses a `StairsDown`/`StairsUp` cell into the
    /// paired cell one rung over (`stairs_pair_by_coordinate_across_
    /// adjacent_rungs` pins that pairing), so the search graph matches the
    /// plan's own. Positive control counts multi-node levels so the
    /// cross-region connector is actually exercised.
    ///
    /// **The standable predicate is the `movement_mode` seam since The
    /// Brattice**, not a list of kinds: `Threshold` and `Drop` walk, `Deep`
    /// swims, and this sweep asks the resident's question — is every cell a
    /// body could occupy reachable — rather than re-deriving a kind list
    /// that a new variant silently falsifies. The `Drop` down-edge joins the
    /// stair edges below for the same reason (spec §3.5).
    #[test]
    fn every_character_engine_keeps_every_level_connected() {
        use hornvale_kernel::Band;
        use hornvale_terrain::CaveKind;
        use hornvale_worldgen::chamber::ChamberOrigin;
        use std::collections::{BTreeSet, VecDeque};

        let rungs = [Band::Undercroft, Band::Shallows, Band::Deeps];
        let origins = [
            ChamberOrigin::Found,
            ChamberOrigin::Made,
            ChamberOrigin::Found,
        ];
        let depths_m = [20.0, 60.0, 120.0];
        let mut composite_levels_probed = 0;
        for cave_kind in [CaveKind::Karst, CaveKind::LavaTube, CaveKind::Fracture] {
            for character in hornvale_worldgen::character::CHARACTERS {
                for s in 0..12u64 {
                    let plan = hornvale_worldgen::circuit::plan_descent(
                        Seed(s),
                        hornvale_kernel::Vertex(0),
                        &rungs,
                        cave_kind,
                        *character,
                    );
                    let levels = generate_descent_for_character(
                        &rungs,
                        cave_kind,
                        &origins,
                        &depths_m,
                        500.0,
                        *character,
                        &plan,
                        Seed(s),
                    );
                    let standables: Vec<BTreeSet<Cell>> = levels
                        .iter()
                        .enumerate()
                        .map(|(i, level)| {
                            if level.leaf_styles.len() > 1 {
                                composite_levels_probed += 1;
                            }
                            let standable: BTreeSet<Cell> = level
                                .cells
                                .iter()
                                .filter(|(_, k)| movement_mode(*k).is_some())
                                .map(|(c, _)| c)
                                .collect();
                            assert!(
                                !standable.is_empty(),
                                "{character:?} seed {s} level {i}: no standable cells"
                            );
                            standable
                        })
                        .collect();
                    let total: usize = standables.iter().map(|s| s.len()).sum();
                    let start = (
                        0usize,
                        *standables[0].iter().next().expect("non-empty above"),
                    );
                    let mut seen: BTreeSet<(usize, Cell)> = BTreeSet::new();
                    let mut queue = VecDeque::new();
                    seen.insert(start);
                    queue.push_back(start);
                    while let Some((lvl, Cell(x, y))) = queue.pop_front() {
                        for (dx, dy) in [(1, 0), (-1, 0), (0, 1), (0, -1)] {
                            let next = (lvl, Cell(x + dx, y + dy));
                            if standables[lvl].contains(&next.1) && seen.insert(next) {
                                queue.push_back(next);
                            }
                        }
                        let kind = levels[lvl].cells.get(Cell(x, y));
                        if kind == Some(LevelCellKind::StairsDown) && lvl + 1 < levels.len() {
                            let below = (lvl + 1, Cell(x, y));
                            if standables[lvl + 1].contains(&below.1) && seen.insert(below) {
                                queue.push_back(below);
                            }
                        }
                        if kind == Some(LevelCellKind::StairsUp) && lvl > 0 {
                            let above = (lvl - 1, Cell(x, y));
                            if standables[lvl - 1].contains(&above.1) && seen.insert(above) {
                                queue.push_back(above);
                            }
                        }
                        // A chute is one-way for a body that cannot fly, and
                        // the descent is walked downward from rung 0, so the
                        // lip's DOWN edge is the one this sweep needs (The
                        // Brattice, spec §3.5).
                        if kind == Some(LevelCellKind::Drop) && lvl + 1 < levels.len() {
                            let below = (lvl + 1, Cell(x, y));
                            if standables[lvl + 1].contains(&below.1) && seen.insert(below) {
                                queue.push_back(below);
                            }
                        }
                    }
                    assert_eq!(
                        seen.len(),
                        total,
                        "{character:?}/{cave_kind:?} seed {s}: {} of {} standable cells \
                         unreachable across the whole descent",
                        total - seen.len(),
                        total
                    );
                }
            }
        }
        assert!(
            composite_levels_probed > 0,
            "this sweep never generated a composite level — the cross-region \
             connector would go unexercised"
        );
    }

    #[test]
    /// claim: invariant(seed: 0..20 x all kinds x all origins) — every
    /// generated level's walkable cells form exactly one connected
    /// component, for every seed in the range, swept across every
    /// `CaveKind` x `ChamberOrigin` combination.
    /// This is the property Task 2's original design silently failed to
    /// guarantee for any level with more than one leaf.
    ///
    /// **Swept over all 6 `(CaveKind, ChamberOrigin)` combinations**, not
    /// just `(Fracture, Found)` — a prior version of this sweep hardcoded
    /// that one pair, and `choose_leaf_style`'s `Found`-origin base chance
    /// (0.10) blended against the neutral 0.5 bias put every leaf on
    /// `AngularRooms`/`RoomsAndCorridors`, so `CellularCave` and `Tunneler`
    /// were never exercised here at all — exactly the blind spot that let
    /// Task 10's `CellularCave`-disconnected-caverns defect go uncaught
    /// until an unrelated integration test happened to hit it. Also asserts
    /// a positive control (`composite_levels_probed > 0`): the
    /// cross-leaf-connectivity property this sweep guards (Task 9) is only
    /// meaningfully tested if at least some generated levels actually have
    /// more than one leaf.
    ///
    /// **"Walkable" counts `StairsDown`/`StairsUp` too, since a review of
    /// this campaign's stair-repair fix (`reconnect_region`, scoped to its
    /// OWN region only, never the whole level).** A single-rung plan is
    /// always its own terminus, so `generate_level_with_origin` can place
    /// a dangling stair even on a bare direct call like this one — and a
    /// Floor/Flooded-only definition would misread a corridor with a stair
    /// in its middle as two disconnected halves, when `movement_mode`
    /// already answers `Walk` for a stair the same as a floor. Widening
    /// the set to match matters here specifically: before the repair fix
    /// scoped correctly, a whole-level bypass silently carved a corridor
    /// THROUGH a neighbouring region to keep this narrower definition
    /// happy — the very spec §3.3 violation
    /// `unlinked_neighbours_keep_their_wall` now pins. This is the fix on
    /// the test side, not a second violation on the code side.
    ///
    /// **Since The Brattice the predicate IS the seam** — `movement_mode(k)
    /// .is_some()` rather than a list of kinds. This is the RESIDENT's view,
    /// deliberately: `Deep` is passable (to a swimmer), `Threshold` and
    /// `Drop` walk, and a rule written against the predicate survives the
    /// next kind exactly as `movement_mode`'s own doc argues. A rule written
    /// against the variant list would have gone red the day a squeeze landed
    /// in the middle of every corridor, saying "disconnected" about a level
    /// nothing had disconnected.
    fn every_walkable_cell_is_reachable_from_every_other() {
        use hornvale_kernel::Band;
        use hornvale_terrain::CaveKind;
        use hornvale_worldgen::chamber::ChamberOrigin;
        use std::collections::{BTreeSet, VecDeque};

        let mut composite_levels_probed = 0;
        let extent = generate_level_extent(Band::Undercroft);
        for cave_kind in [CaveKind::Karst, CaveKind::LavaTube, CaveKind::Fracture] {
            for origin in [ChamberOrigin::Found, ChamberOrigin::Made] {
                for seed_value in 0..20u64 {
                    let plan = one_rung_plan(seed_value, cave_kind);
                    let level = generate_level_with_origin(
                        extent,
                        cave_kind,
                        origin,
                        Character::WildCave,
                        NEUTRAL_WORKED_BIAS,
                        &plan,
                        0,
                        Seed(seed_value),
                    );
                    if level.leaf_styles.len() > 1 {
                        composite_levels_probed += 1;
                    }
                    let walkable: BTreeSet<Cell> = level
                        .cells
                        .iter()
                        .filter(|(_, k)| movement_mode(*k).is_some())
                        .map(|(c, _)| c)
                        .collect();
                    let Some(&start) = walkable.iter().next() else {
                        continue; // a degenerate all-wall level has nothing to check
                    };
                    let mut seen = BTreeSet::new();
                    let mut queue = VecDeque::new();
                    seen.insert(start);
                    queue.push_back(start);
                    while let Some(Cell(x, y)) = queue.pop_front() {
                        for (dx, dy) in [(1, 0), (-1, 0), (0, 1), (0, -1)] {
                            let next = Cell(x + dx, y + dy);
                            if walkable.contains(&next) && seen.insert(next) {
                                queue.push_back(next);
                            }
                        }
                    }
                    assert_eq!(
                        seen.len(),
                        walkable.len(),
                        "{cave_kind:?}/{origin:?} seed {seed_value}: {} of {} walkable cells unreachable from {start:?}",
                        walkable.len() - seen.len(),
                        walkable.len()
                    );
                }
            }
        }
        assert!(
            composite_levels_probed > 0,
            "this sweep never generated a level with more than one leaf — the \
             cross-leaf connectivity property (Task 9) this test guards would \
             pass vacuously"
        );
    }
}
