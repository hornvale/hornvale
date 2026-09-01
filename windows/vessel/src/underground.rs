//! The session's position within a generated underworld descent (The
//! Gallery, Task 3).
//!
//! Before this task, `delve` set a bucket: `Session.underground` held a
//! single [`hornvale_worldgen::chamber::Chamber`] — an address and a rock
//! type, nothing a possession could stand *in*. This module makes it a real
//! place: [`Underground::enter`] builds a whole descent through
//! [`generate_descent_for_character`] and stands the possession on a real
//! cell of a real generated level.
//!
//! `FRAME`-tier in its entirety (decision 0069): built fresh on every
//! `delve`, never serialized. A world is a seed plus a ledger; nothing here
//! is either.

use hornvale_kernel::{Band, KindId, Seed, Vertex};
use hornvale_locale::Compass;
use std::collections::BTreeSet;

use crate::lattice::{Cell, Rect};
use crate::underworld_level::{Level, LevelCellKind, generate_descent_for_character};

/// Underground's own CORNER refusal — the same geometry
/// `INDOOR_CORNER_REFUSAL` states one band over (`session.rs`): passing
/// between two solid things that meet at a point is not a way through, and
/// rock is no different from built fabric about it.
///
/// **It used to refuse every diagonal, and both halves of the reason it gave
/// have lapsed.** The old text ("There is no slipping through a corner down
/// here either; try north, south, east or west") rested on
/// [`crate::session::cell_delta`] being orthogonal only. That table is total
/// now (spec section 3.1) and a diagonal step underground is ordinary
/// movement; what survives is the narrow claim about a closed corner, asked
/// cell by cell through [`crate::lattice::diagonal_is_blocked`]. It no longer
/// names four bearings that work, because which bearings work is a fact about
/// the cell stood on: with one flank open the diagonal is walkable, and a
/// sentence naming a fixed four would be false there.
const UNDERGROUND_CORNER_REFUSAL: &str =
    "Rock meets rock at that corner; there is no slipping between them down here either.";

/// The physical reason a rock cell refuses a lateral step underground.
/// Names no verb and no movement mode — a parse complaint this is not.
const UNDERGROUND_ROCK_REFUSAL: &str =
    "Solid rock closes off the way; there is no path through it.";

/// The physical reason [`Underground::peek_stairs`] refuses when the
/// current cell is not a stairs cell at all (The Gallery, Task 5). Never
/// surfaced through `Session::take_stairs` in practice — that caller checks
/// the current cell's kind against the direction it wants before ever
/// asking `peek_stairs`, and refuses with a direction-specific sentence of
/// its own instead — but `peek_stairs`/`take_stairs` are real seams, and
/// `session.rs`'s own `peek_stairs_refuses_off_any_stairs_cell` reaches this
/// arm directly, the same way `rock_refuses_a_step_with_a_physical_reason`
/// reaches `Underground::peek`'s seam rather than through `Session::handle`.
const NOT_ON_STAIRS_REFUSAL: &str = "There is no stairway underfoot to take.";

/// The physical reason the descent's own deepest rung refuses a `StairsDown`
/// (The Gallery, Task 5). `place_connections`
/// (`windows/vessel/src/underworld_level/mod.rs`) cuts a `StairsDown` cell
/// into every rung unconditionally — the bottom rung included — but the
/// generated descent itself stops at the last rung [`Underground::enter`]
/// built, so that rung's own down-stairs lead to a rung nothing has
/// generated yet.
const STAIRS_LEAD_NOWHERE_REFUSAL: &str =
    "The stairs continue down into unbroken dark, but nothing has delved that far yet.";

/// Every habitation rung the delve ladder names, [`Band::Surface`]
/// excluded — the same filter
/// `windows/vessel/tests/suite/underworld_level_generation.rs`'s
/// `measure_flooded_cell_reachability_across_the_descent` applies. The
/// index into this list *is* [`Underground::rung`]: `descent[i]` is always
/// the level [`Underground::enter`] generated for `habitation_rungs()[i]`.
fn habitation_rungs() -> Vec<Band> {
    hornvale_terrain::rungs()
        .iter()
        .copied()
        .filter(|&rung| rung != Band::Surface)
        .collect()
}

/// One bit per cell of a rung's extent, remembering which cells the
/// possession has ever seen there (The Gallery, Task 6; spec §3.5).
///
/// Row-major over `extent`: cell `(x, y)` is bit
/// `(y - extent.y) * extent.w + (x - extent.x)`, packed 64 cells to a
/// `u64`. The largest rung is 60x34 = 2,040 cells = 255 bytes; a five-rung
/// descent is about 1.2 KB (spec §3.5).
///
/// **Monotone by construction** (spec §4.1.2's acceptance 4b). The only
/// mutator, [`SeenBits::mark_all`], only ever sets bits — there is no
/// clearing operation anywhere on this type — so "a cell once seen is
/// never un-seen within a descent" is a fact about the representation, not
/// a rule any caller has to remember to uphold.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SeenBits {
    /// The rung's own extent — the same [`Rect`] its [`Level::extent`]
    /// carries.
    extent: Rect,
    /// The packed bits, 64 cells per word, row-major over `extent`.
    bits: Vec<u64>,
}

impl SeenBits {
    /// A fresh, entirely-unseen bitset over `extent`.
    pub(crate) fn new(extent: Rect) -> SeenBits {
        let cells = (extent.w.max(0) as usize).saturating_mul(extent.h.max(0) as usize);
        let words = cells.div_ceil(64);
        SeenBits {
            extent,
            bits: vec![0u64; words],
        }
    }

    /// `c`'s bit index within `bits`, or `None` if `c` lies outside
    /// `extent`.
    fn index_of(&self, c: Cell) -> Option<usize> {
        if !self.extent.contains(c) {
            return None;
        }
        let dx = (c.0 - self.extent.x) as usize;
        let dy = (c.1 - self.extent.y) as usize;
        Some(dy * self.extent.w as usize + dx)
    }

    /// Has `c` ever been seen? `false` for any cell outside `extent` — the
    /// same tolerance [`crate::underworld_level::CellGrid::get`] gives an
    /// out-of-bounds read, rather than a panic.
    ///
    /// **Live in production since Task 7.** `Session::underground_level`
    /// (`session.rs`) reads this once per cell of the rung's own extent,
    /// through the closure it hands to [`crate::level_doc::level_of`], to
    /// decide `remembered` (seen, not currently lit) from `never-seen`
    /// (omitted from the document entirely — spec §4.1.1) for every cell
    /// the current shadowcast does not itself light.
    /// type-audit: bare-ok(flag: return)
    pub fn saw(&self, c: Cell) -> bool {
        match self.index_of(c) {
            Some(bit) => self.bits[bit / 64] & (1u64 << (bit % 64)) != 0,
            None => false,
        }
    }

    /// Mark every one of `cells` seen. A cell outside `extent` is silently
    /// ignored. Only ever SETS bits, never clears any — see the type's own
    /// doc for why that is the whole of the monotonicity guarantee.
    pub fn mark_all(&mut self, cells: &BTreeSet<Cell>) {
        for &c in cells {
            if let Some(bit) = self.index_of(c) {
                self.bits[bit / 64] |= 1u64 << (bit % 64);
            }
        }
    }
}

/// The session's position within one cave system's generated descent (The
/// Gallery, Task 3).
///
/// **The struct's fields are `pub(crate)`, not private.** It lives in its
/// own module while `session.rs`'s test module reaches into it directly —
/// the same reason `Inside` (`session.rs`) gets away with bare private
/// fields is exactly the reason this one cannot: `Inside` is declared
/// inside the module that reads it, this is not.
pub(crate) struct Underground {
    /// One generated level per habitation rung, in [`habitation_rungs`]
    /// order: `descent[i]` is the level for `habitation_rungs()[i]`.
    pub(crate) descent: Vec<Level>,
    /// Which element of `descent` the possession currently occupies.
    /// [`Underground::enter`] is the only constructor and always enters at
    /// the top (`rung == 0`) — but that is no longer this field's whole
    /// story: [`Underground::take_stairs`] (The Gallery, Task 5) is the
    /// project's first and only mutator of `rung`, moving it up or down one
    /// rung per stairway taken, so a live `Underground` cannot be assumed to
    /// sit at rung `0` merely because it was once true at construction. An
    /// index into `habitation_rungs()` as well as into `descent`; the two
    /// can never disagree because both are sized and ordered from the same
    /// `habitation_rungs()` call.
    pub(crate) rung: usize,
    /// Which cell of `descent[rung]` the possession stands on. Always a
    /// `Floor` or `Flooded` cell at the moment [`Underground::enter`]
    /// places it — never `Wall`, `StairsDown` or `StairsUp`.
    pub(crate) cell: Cell,
    /// The surface vertex this whole descent hangs beneath — the same
    /// vertex [`Underground::enter`]’s caller (`Session::delve_at`) already
    /// resolved the cave through. Every rung of `descent` shares this one
    /// vertex; only the evaluation depth ([`Underground::depths_m`]) varies
    /// between them. Kept so a later read of this chamber’s own conditions
    /// (spec §3.6, Task 11) can re-derive `terrain.geothermal_gradient_at`,
    /// `terrain.material_at` and the rest from the SAME point `enter` itself
    /// read, rather than a second, independently-chosen vertex.
    pub(crate) vertex: Vertex,
    /// The seed the descent was generated from (the world's own seed —
    /// [`Underground::enter`] derives nothing else). Carried alongside the
    /// generated content the same way [`crate::lattice::Lattice`]'s
    /// embedding seed rides beside `Inside`'s own `lattice`, so a later
    /// verb that needs to reproduce or extend this descent has it in hand
    /// rather than threading it through a second parameter.
    ///
    /// **Still unused as of Task 6.** The fog-of-war bitset ([`SeenBits`])
    /// turned out not to need it either — spec §3.5 is explicit that fog is
    /// playthrough history, not something a `(seed, address)` re-derives,
    /// so `seen` is built empty over each rung's extent and mutated by
    /// walking rather than rebuilt from this field. Kept for the reason its
    /// first paragraph gives: a later verb that reproduces or extends this
    /// descent has it in hand rather than threading it through a second
    /// parameter.
    #[allow(dead_code)]
    pub(crate) seed: Seed,
    /// One [`SeenBits`] per element of `descent`, same indexing: `seen[i]`
    /// is rung `i`'s own remembered set. Session-lifetime and per-descent
    /// (spec §3.5's lifetime cut): built fresh, all-unseen, by
    /// [`Underground::enter`], and discarded along with the rest of this
    /// struct the moment the possession climbs out.
    pub(crate) seen: Vec<SeenBits>,
    /// One evaluation depth in metres per element of `descent`, same
    /// indexing: `depths_m[i]` is `descent[i]`'s own
    /// `hornvale_terrain::rung_evaluation_depth_m` result, exactly as
    /// [`Underground::enter`] computed it to build that rung.
    ///
    /// **Kept alongside `descent` since Task 7**, rather than recomputed at
    /// snapshot time: the depth is a function of the entrance vertex's own
    /// geothermal gradient and the cave's `depth_reach_m`, and by the time a
    /// snapshot reads it the possession has walked away from that vertex —
    /// recomputing would mean carrying both inputs forward as a second copy
    /// of exactly what this vector already holds. `Session::
    /// underground_level` reads `depths_m[rung]` to fill
    /// `SessionLevel::depth_m`.
    pub(crate) depths_m: Vec<f64>,
}

impl Underground {
    /// Enter a cave system at its topmost rung: build the whole descent
    /// through [`generate_descent_for_character`] and stand the possession
    /// on the entrance rung's first standable cell — the first `Floor` or
    /// `Flooded` cell in the level's own ascending-`(x, y)` order
    /// ([`crate::underworld_level::CellGrid::iter`]).
    ///
    /// **The descent's inputs are the production recipe**, copied the way
    /// `windows/vessel/tests/suite/underworld_level_generation.rs`'s
    /// `measure_flooded_cell_reachability_across_the_descent` already
    /// copies `windows/worldgen/src/lib.rs`'s
    /// `subterranean_substrate_at_rung_with_cave`: gradient and porosity
    /// read off `terrain` at `vertex`, the water table from
    /// `hornvale_terrain::water_table_depth_m`, and a per-rung evaluation
    /// depth from `hornvale_terrain::rung_evaluation_depth_m`. `cave.kind`
    /// names the natural-leaf algorithm (never a hardcoded `CaveKind`).
    /// Every rung's origin is
    /// [`hornvale_worldgen::chamber::ChamberOrigin::Found`] — the shipped
    /// path constructs no `ChamberOverrides`, so `Made` is unreachable
    /// here — and the character is
    /// [`hornvale_worldgen::character::Character::WildCave`], matching
    /// [`crate::underworld_level::generate_descent`]'s own hardcoded
    /// value.
    ///
    /// Both `terrain` and `cave` come from the same terrain handle the
    /// caller (`Session::delve_at`) already resolved: no second,
    /// independently-chosen lookup is introduced here.
    ///
    /// Panics if the entrance rung's generated level has no `Floor` or
    /// `Flooded` cell at all. That is an invariant of the generator, not a
    /// case this task designs a refusal for — Task 9's own connectivity
    /// sweep (`every_walkable_cell_is_reachable_from_every_other`,
    /// `windows/vessel/src/underworld_level/mod.rs`) holds it for every
    /// level this generator produces.
    pub(crate) fn enter(
        terrain: &hornvale_terrain::GeneratedTerrain,
        vertex: hornvale_kernel::Vertex,
        cave: hornvale_terrain::Cave,
        seed: Seed,
    ) -> Underground {
        let rungs = habitation_rungs();
        let gradient = terrain.geothermal_gradient_at(vertex);
        let porosity = terrain.material_at(vertex).porosity;
        let height_asl_m = terrain
            .elevation_at(vertex)
            .above(terrain.sea_level())
            .get();
        let water_table_m = hornvale_terrain::water_table_depth_m(
            terrain.drainage_at(vertex),
            porosity,
            height_asl_m,
        );
        let depths_m: Vec<f64> = rungs
            .iter()
            .map(|&rung| {
                hornvale_terrain::rung_evaluation_depth_m(rung, gradient, cave.depth_reach_m)
                    .expect("every non-Surface rung has an evaluation depth")
            })
            .collect();
        let origins = vec![hornvale_worldgen::chamber::ChamberOrigin::Found; rungs.len()];
        let descent = generate_descent_for_character(
            &rungs,
            cave.kind,
            &origins,
            &depths_m,
            water_table_m,
            hornvale_worldgen::character::Character::WildCave,
            seed,
        );
        let cell = descent[0]
            .cells
            .iter()
            .find(|(_, k)| matches!(k, LevelCellKind::Floor | LevelCellKind::Flooded))
            .map(|(c, _)| c)
            .expect(
                "a generated level has at least one standable cell \
                 (Task 9's connectivity invariant)",
            );
        // Task 6's fog-of-war: one all-unseen bitset per rung, sized to
        // that rung's own extent (deeper rungs are wider — see
        // `generate_level_extent`). `Underground::enter` itself marks
        // nothing — the CALLER does: `Session::delve_at` marks the
        // entrance's own surroundings immediately after this constructor
        // returns (Fix round 1, spec §3.5 amended by commit f6051a9c3:
        // every arrival marks, not just a lateral step), so by the time a
        // live session sees this descent, rung 0 already remembers
        // something.
        let seen = descent
            .iter()
            .map(|level| SeenBits::new(level.extent))
            .collect();
        Underground {
            descent,
            rung: 0,
            cell,
            vertex,
            seed,
            seen,
            depths_m,
        }
    }

    /// The level the possession currently occupies — `descent[rung]`.
    pub(crate) fn level(&self) -> &Level {
        &self.descent[self.rung]
    }

    /// Which band `self.rung` names — `habitation_rungs()[rung]`, the same
    /// index [`Underground::enter`] built `descent` against.
    ///
    /// **Live in production since Task 7**: `Session::underground_level`
    /// reads it to fill `SessionLevel::rung`, hand-mapped to a wire string
    /// through `level_doc`'s own `band_wire_name` rather than serialized
    /// from this `Band`'s `Debug` text — the same reason `plan::entry_for`
    /// hand-maps `CellKind` instead of deriving its wire string, since a
    /// wire value is a contract and `Debug` output is not one (`Nadir` was
    /// `Sunless` until The Stope renamed it, with no wire consumer to
    /// notice either way).
    pub(crate) fn rung_band(&self) -> Band {
        habitation_rungs()[self.rung]
    }

    /// The would-be destination of a compass step underground, WITHOUT
    /// moving there (The Gallery, Task 4; reshaped in Fix round 1).
    ///
    /// **The reversal `UNDERGROUND_LATERAL_REFUSAL` (`session.rs`) owed the
    /// moment this task landed.** That constant's own doc said "there is
    /// nowhere down here for a bearing to mean" — true only while the cave
    /// lattice reached no further than its entrance chamber; Task 3 gave it
    /// a whole generated level, which is what this method (with
    /// [`Underground::commit_step`]) walks.
    ///
    /// Follows the indoor compass step's own precedent
    /// (`Session::step`, `session.rs:3413`) rule for rule:
    ///
    /// 1. **A diagonal through a two-walled corner is refused**
    ///    ([`UNDERGROUND_CORNER_REFUSAL`]) before anything is looked up, asked
    ///    through [`crate::lattice::diagonal_is_blocked`] with this band's own
    ///    passability oracle. **It used to refuse every diagonal**, on the
    ///    strength of [`crate::session::cell_delta`] being orthogonal only;
    ///    that table is total now (spec section 3.1), so the refusal is the
    ///    corner it was always really about (spec section 3.3) and nothing
    ///    else.
    /// 2. **An impassable target is refused with a physical reason**
    ///    ([`UNDERGROUND_ROCK_REFUSAL`]), asked through
    ///    [`crate::underworld_level::movement_mode`] rather than compared
    ///    against `LevelCellKind::Wall` directly — the same rule
    ///    `CellKind::passable`'s own doc gives, so the refusal survives the
    ///    day a new impassable kind arrives.
    ///
    /// **Split from a single all-in-one `step` (Fix round 1, review finding
    /// 2) so a caller can charge a cost BEFORE the move lands**, the same
    /// order the indoor compass step's own precedent uses (diagonal
    /// refused, then impassable refused, then `charge_within_room`, then —
    /// only once that succeeds — the cell mutates). The original
    /// all-in-one `step` moved the possession as part of computing its
    /// outcome, so a caller could only charge AFTER the move had already
    /// landed — silently making an underground step free and letting the
    /// possession move on a refused clock besides. `Session::
    /// step_underground` is the one caller: it calls this, charges, and
    /// only then calls [`Underground::commit_step`].
    pub(crate) fn peek(&self, dir: Compass) -> Result<Cell, &'static str> {
        let delta = crate::session::cell_delta(dir);
        // This band's passability oracle, given to the shared corner rule: a cell
        // is open when `movement_mode` has an answer for its kind, which is the
        // same question step 2 below asks about the destination — never a
        // comparison against `LevelCellKind::Wall`, so both survive the day a new
        // impassable kind arrives.
        let open = |c: Cell| {
            self.descent[self.rung]
                .cells
                .get(c)
                .and_then(crate::underworld_level::movement_mode)
                .is_some()
        };
        if crate::lattice::diagonal_is_blocked(self.cell, delta, open) {
            return Err(UNDERGROUND_CORNER_REFUSAL);
        }
        let target = Cell(self.cell.0 + delta.0, self.cell.1 + delta.1);
        if !open(target) {
            return Err(UNDERGROUND_ROCK_REFUSAL);
        }
        Ok(target)
    }

    /// Commit a step already validated by [`Underground::peek`]: moves the
    /// possession to `target` (already known passable) and reports whether
    /// it landed on a stairs cell. Never itself refuses — `peek` is the only
    /// gate — which is what makes it safe to call only after a charge has
    /// already succeeded.
    pub(crate) fn commit_step(&mut self, target: Cell) -> StepOutcome {
        let kind = self.descent[self.rung].cells.get(target);
        self.cell = target;
        if matches!(
            kind,
            Some(LevelCellKind::StairsDown) | Some(LevelCellKind::StairsUp)
        ) {
            StepOutcome::NeedsStairs
        } else {
            StepOutcome::Moved
        }
    }

    /// Where taking the stairs at the possession's CURRENT cell would lead,
    /// without moving there (The Gallery, Task 5) — the same peek/commit
    /// split [`Underground::peek`]/[`Underground::commit_step`] already draw
    /// one rung over, so a caller can charge a cost before the move lands and
    /// never move the possession on a refused clock.
    ///
    /// **The direction is read off the CURRENT cell's own kind, never a
    /// parameter**: a `StairsDown` cell means descend, a `StairsUp` cell
    /// means ascend, anything else refuses. A caller that wants a SPECIFIC
    /// direction (the `down`/`up` verbs, `session.rs`) checks the current
    /// cell's kind against the one it wants BEFORE ever calling this — this
    /// method alone cannot refuse "wrong direction", only "no direction at
    /// all" or "no destination for the direction there is".
    ///
    /// **The landing cell is the connecting stairway's own cell on the far
    /// side, never a fresh scan for "somewhere standable"**: descending from
    /// rung `n` lands on rung `n + 1`'s own `StairsUp` cell, and ascending
    /// from rung `n` lands on rung `n - 1`'s own `StairsDown` cell — the same
    /// physical stairway, named from its other end. Both are guaranteed to
    /// exist by the generator's own invariant — every rung has exactly one
    /// `StairsDown` cell, and every rung but the first has exactly one
    /// `StairsUp` cell (`place_connections`' `has_up = i > 0`;
    /// `windows/vessel/src/underworld_level/mod.rs`), asserted across 200
    /// seeds by that module's own `stairs_down_and_stairs_up_never_share_a_
    /// cell` — so descending always has somewhere to land on any rung but
    /// the last, and ascending always has somewhere to land on any rung but
    /// the first (which has no `StairsUp` cell to be standing on in the
    /// first place, so that arm is never reached from rung `0`).
    ///
    /// Refuses when the current cell is not a stairs cell at all
    /// ([`NOT_ON_STAIRS_REFUSAL`]), or when the current cell is a
    /// `StairsDown` on the descent's own deepest rung — the one rung
    /// `place_connections` still cuts a down-stairs into (it never
    /// special-cases the last rung) even though [`Underground::enter`]
    /// generated nothing beneath it ([`STAIRS_LEAD_NOWHERE_REFUSAL`]).
    pub(crate) fn peek_stairs(&self) -> Result<(usize, Cell), &'static str> {
        match self.descent[self.rung].cells.get(self.cell) {
            Some(LevelCellKind::StairsDown) => {
                let next = self.rung + 1;
                if next >= self.descent.len() {
                    return Err(STAIRS_LEAD_NOWHERE_REFUSAL);
                }
                let landing = self.descent[next]
                    .cells
                    .iter()
                    .find(|(_, k)| matches!(k, LevelCellKind::StairsUp))
                    .map(|(c, _)| c)
                    .expect(
                        "every rung but the first has a StairsUp cell \
                         (place_connections' has_up = i > 0), and `next` \
                         is never 0",
                    );
                Ok((next, landing))
            }
            Some(LevelCellKind::StairsUp) => {
                let next = self.rung.checked_sub(1).expect(
                    "a StairsUp cell only exists at rung > 0 \
                     (place_connections' has_up = i > 0), so its own rung \
                     always has a predecessor",
                );
                let landing = self.descent[next]
                    .cells
                    .iter()
                    .find(|(_, k)| matches!(k, LevelCellKind::StairsDown))
                    .map(|(c, _)| c)
                    .expect(
                        "every rung has a StairsDown cell — place_connections \
                         cuts one unconditionally, the last rung included",
                    );
                Ok((next, landing))
            }
            _ => Err(NOT_ON_STAIRS_REFUSAL),
        }
    }

    /// Take the stairs at the possession's current cell (The Gallery, Task
    /// 5): commit a move [`Underground::peek_stairs`] already validated,
    /// moving the possession to the destination rung and cell and returning
    /// the new rung's own [`Band`] (`habitation_rungs()[rung]`, the same
    /// index [`Underground::rung_band`] reads).
    ///
    /// `None` on any of [`Underground::peek_stairs`]'s own refusals — never
    /// itself moves the possession on a call that would have refused, which
    /// is what makes it safe for a caller that does not need to charge
    /// anything (this module's own tests are exactly that caller) to call
    /// directly, and safe for a caller that DOES need to charge
    /// (`Session::take_stairs`, `session.rs`) to call only after peeking and
    /// charging first — the same division
    /// [`Underground::peek`]/[`Underground::commit_step`] already draw.
    pub(crate) fn take_stairs(&mut self) -> Option<Band> {
        let (next, landing) = self.peek_stairs().ok()?;
        self.rung = next;
        self.cell = landing;
        Some(habitation_rungs()[self.rung])
    }
}

// --- Task 11: inhabitants (spec §3.6) ---------------------------------

/// Below this, a species' fit is treated as "cannot be fed here" rather than
/// merely "poorly fed" — the difference between a chamber holding a marginal
/// resident and holding nobody at all (spec §3.6: "a chamber that can feed
/// nothing holds nothing"). Deliberately small: at a chamber near either
/// subterranean species' own preferred elevation, [`inhabitant_fit`] reads
/// well clear of this line (rust-monster's own elevation optimum alone
/// clears `0.2`; a fed xorn's energy term alone clears `0.3` — see
/// [`who_is_underground_derives_from_the_chambers_own_conditions`]'s
/// measured baseline), so this threshold exists to catch the genuinely
/// hostile case — an elevation far outside every authored niche, combined
/// with a chemotroph starved of energy — not to prune the ordinary middle
/// of the distribution. A calibration knob, not a physical constant;
/// nothing in this campaign measures where it should sit more precisely.
/// type-audit: bare-ok(ratio)
const INHABITANT_FIT_THRESHOLD: f64 = 0.05;

/// One species' fit at one chamber, `[0, 1]` — a blend of
/// [`hornvale_worldgen::tolerance_liebig`]'s environmental score and the
/// chamber's own energy reading, weighted by how much of that species' diet
/// the energy sources actually cover.
///
/// **The weight is the species' own authored `CHEMOSYNTHATE` niche weight,
/// not a second axis dot product.** The full genesis-time capacity
/// calculation weighs `CHEMOSYNTHATE` alongside six other supply axes via
/// each species' authored niche weights (`windows/worldgen/src/lib.rs`'s
/// `per_species_capacity_at`); none of those other six fields exist at a
/// per-turn query, and the subterranean roster does not need them — THE
/// SOURCES already authored `xorn`'s niche at `0.65 MINERAL / 0.35
/// CHEMOSYNTHATE` (`domains/species/src/lib.rs`'s `biosphere_registry`),
/// which is the exact number this reads back with `bio.niche.
/// weight(CHEMOSYNTHATE)`. `rust-monster`'s niche carries no `CHEMOSYNTHATE`
/// weight at all (`0.0`, pure `MINERAL`), so for it this collapses to
/// `condition_fit` exactly — untouched by how rich or poor the rock's
/// chemical sources are, which is correct biology (it eats metal, not a
/// chemical gradient) and needs no special-casing to fall out of the
/// formula.
///
/// **A weighted blend, not a Liebig `min`, and deliberately so.** A first
/// draft gated a chemotroph with `condition_fit.min(energy)` — but xorn's
/// OWN elevation-tolerance ceiling (`devotion: 0.10`) already caps its
/// `condition_fit` around `0.10` everywhere (the same "elevation binds"
/// pathology `tolerance_liebig`'s own doc discloses for goblin/gnoll/human),
/// so a `min` can only ever SHRINK that already-small number — it can never
/// lift xorn's fit above rust-monster's (whose own elevation devotion,
/// `0.25`, gives it a materially higher ceiling), and "a chamber whose
/// dominant energy source differs should be able to hold a different
/// creature than one next to it" (spec §3.6) would never actually be
/// witnessed by any real chamber. The blend lets a well-fed chemotroph's
/// score rise on the energy term rather than merely fail to fall on it —
/// see [`who_is_underground_derives_from_the_chambers_own_conditions`] for
/// the measured crossover this buys, at a real, unremarkable chamber
/// (height 0 m, temperature 5 °C, moisture 0.85).
fn inhabitant_fit(
    bio: &hornvale_species::BiosphereTraits,
    substrate: &hornvale_worldgen::Substrate,
    energy: f64,
) -> f64 {
    let floor_buf = hornvale_kernel::sovereignty_floor(bio.mass, bio.potency);
    let condition_fit =
        hornvale_worldgen::tolerance_liebig(&bio.condition_niche, substrate, floor_buf);
    let chemo_weight = bio
        .niche
        .weight(hornvale_kernel::CHEMOSYNTHATE)
        .clamp(0.0, 1.0);
    condition_fit * (1.0 - chemo_weight) + energy.clamp(0.0, 1.0) * chemo_weight
}

/// Which subterranean species best fits one chamber's own `substrate` and
/// `energy` reading (spec §3.6), or `None` if nothing in the subterranean
/// roster ([`hornvale_species::habitat_realm_registry`], filtered to
/// [`hornvale_species::HabitatRealm::Subterranean`] and, below, to the
/// solitary/gregarious/sessile kinds — no `SocialForm::Settled` PEOPLE)
/// clears [`INHABITANT_FIT_THRESHOLD`] — "a chamber that can feed nothing
/// holds nothing."
///
/// **Pure, and deliberately so**: this is the seam
/// [`who_is_underground_derives_from_the_chambers_own_conditions`]
/// perturbs directly, with no `Underground`, no terrain and no climate in
/// sight — the property under test is "this function's OUTPUT moves when
/// its INPUTS do," which a pure function over two arguments states as
/// plainly as it can be stated.
///
/// Ties (equal fit, to the bit) break on `KindId`'s own `Ord` — the
/// registry's ascending iteration order already gives one, but stating the
/// break explicitly keeps this function's own result independent of
/// whatever order a future larger roster happens to iterate in. `total_cmp`
/// for the float comparison itself (no float-`Ord` shortcuts — CLAUDE.md),
/// a tie-break for the rest.
pub(crate) fn dominant_inhabitant(
    substrate: &hornvale_worldgen::Substrate,
    energy: f64,
) -> Option<KindId> {
    let biosphere = hornvale_species::biosphere_registry();
    hornvale_species::habitat_realm_registry()
        .iter()
        .filter(|(_, realm)| **realm == hornvale_species::HabitatRealm::Subterranean)
        .filter_map(|(kind, _)| biosphere.get(kind).map(|bio| (*kind, bio)))
        // **A PEOPLE is not a wandering monster.** `habitat_realm_registry`
        // gained a third row since this task's brief was written — `drow`
        // (The Radiation, C2d), `SocialForm::Settled` — whose own doc names
        // it "the store's first PEOPLED occupant, and the first row whose
        // consumer is settlement placement rather than a readout." A people
        // is founded through settlement placement, with agency and society;
        // a chamber's derived RESIDENT here is a solitary creature a
        // possession might stumble on, with no such placement behind it. Were
        // this filter absent, `dominant_inhabitant` would print "a drow moves
        // in the dark here" at any qualifying chamber anywhere in the world —
        // the same category error `habitat_realm_registry`'s own doc warns
        // against for a DEPTH-only distinction, one axis over: conflating a
        // people with a monster. `SocialForm::Settled` is exactly the
        // registry's own name for "a settling people" (its own doc), so
        // filtering on it needs no roster maintained here that could drift
        // from that registry's own membership.
        .filter(|(_, bio)| bio.social_form != hornvale_species::SocialForm::Settled)
        .map(|(kind, bio)| (kind, inhabitant_fit(bio, substrate, energy)))
        .filter(|(_, fit)| *fit > INHABITANT_FIT_THRESHOLD)
        .max_by(|(a_kind, a_fit), (b_kind, b_fit)| {
            a_fit.total_cmp(b_fit).then_with(|| a_kind.0.cmp(b_kind.0))
        })
        .map(|(kind, _)| kind)
}

/// This rung's own substrate, energy scalar and dominant source (spec
/// §3.6) — [`hornvale_worldgen::subterranean_substrate`] and
/// [`hornvale_worldgen::energy::{subterranean_energy, dominant_source}`]
/// evaluated at `ug.vertex`/`ug.depths_m[ug.rung]`, the exact point
/// [`Underground::enter`] built this rung for.
///
/// **Re-derives `gradient`/`porosity`/`water_table_m` rather than storing
/// them** — the same three calls [`Underground::enter`] itself makes (see
/// that method's own doc), so a chamber's hydrology cannot disagree with
/// how it was generated. Only `surface.temperature_c`/`surface.height_asl_m`
/// are read from the constructed `surface` reading below;
/// `subterranean_substrate` overwrites `moisture`/`insolation` outright
/// (its own doc), so the placeholder zeros here never reach a species'
/// score.
pub(crate) fn chamber_conditions(
    ug: &Underground,
    terrain: &hornvale_terrain::GeneratedTerrain,
    climate: &hornvale_climate::GeneratedClimate,
) -> (
    hornvale_worldgen::Substrate,
    f64,
    hornvale_worldgen::energy::EnergySource,
) {
    let gradient = terrain.geothermal_gradient_at(ug.vertex);
    let material = terrain.material_at(ug.vertex);
    let porosity = material.porosity;
    let height_asl_m = terrain.elevation_at(ug.vertex).above(terrain.sea_level());
    let water_table_m = hornvale_terrain::water_table_depth_m(
        terrain.drainage_at(ug.vertex),
        porosity,
        height_asl_m.get(),
    );
    let surface = hornvale_worldgen::Substrate {
        temperature_c: climate.mean_temperature_at(ug.vertex).get(),
        moisture: 0.0,
        insolation: 0.0,
        height_asl_m,
    };
    let depth_m = ug.depths_m[ug.rung];
    let substrate = hornvale_worldgen::subterranean_substrate(
        surface,
        depth_m,
        gradient,
        water_table_m,
        porosity,
    );
    let drainage = terrain.drainage_at(ug.vertex);
    let energy = hornvale_worldgen::energy::subterranean_energy(
        &material,
        gradient,
        depth_m,
        substrate.moisture,
        drainage,
    );
    let source = hornvale_worldgen::energy::dominant_source(
        &material,
        gradient,
        depth_m,
        substrate.moisture,
        drainage,
    );
    (substrate, energy, source)
}

/// The chamber's own resident, if the rung's own conditions can feed one
/// (spec §3.6): [`dominant_inhabitant`] scored against [`chamber_conditions`]'s
/// substrate and energy reading, paired with the dominant source that fed
/// it — carried through purely for [`inhabitant_datum`]'s flavour text, the
/// same "retained beside the scalar" role `dominant_source` plays in the
/// energy module itself.
pub(crate) fn chamber_resident(
    ug: &Underground,
    terrain: &hornvale_terrain::GeneratedTerrain,
    climate: &hornvale_climate::GeneratedClimate,
) -> Option<(KindId, hornvale_worldgen::energy::EnergySource)> {
    let (substrate, energy, source) = chamber_conditions(ug, terrain, climate);
    dominant_inhabitant(&substrate, energy).map(|kind| (kind, source))
}

/// Where a chamber's derived resident stands, if the rung has any standable
/// cell at all: the LAST `Floor`/`Flooded` cell in the level's own ascending
/// `(x, y)` order — the mirror image of [`Underground::enter`]'s own
/// entrance-placement rule (the FIRST such cell), chosen so the resident
/// does not, in general, greet the possession at the stairs it just climbed
/// down. A fixed function of the level's own geometry, not of anything the
/// possession does: the resident does not chase the player around the rung
/// from one snapshot to the next.
pub(crate) fn resident_cell(level: &Level) -> Option<Cell> {
    level
        .cells
        .iter()
        .filter(|(_, k)| matches!(k, LevelCellKind::Floor | LevelCellKind::Flooded))
        .map(|(c, _)| c)
        .last()
}

/// The flavour text a chamber's derived resident's mark carries — naming the
/// SPECIES rather than a personal label, because this creature has no
/// entity identity to be consistent with (spec §3.6's own stop condition:
/// this task ships a query, not a placement engine with tracked
/// individuals). `source` is [`hornvale_worldgen::energy::dominant_source`]'s
/// own reading, hand-mapped to a short phrase rather than its `Debug`
/// text — the same discipline [`crate::level_doc::band_wire_name`] and
/// `crate::plan::entry_for` already apply to their own enums, for the same
/// reason: prose a player reads is not obliged to track an internal
/// variant's name.
pub(crate) fn inhabitant_datum(
    kind: KindId,
    source: hornvale_worldgen::energy::EnergySource,
) -> String {
    format!(
        "A {} moves in the dark here, drawn to {}.",
        kind.0,
        source_phrase(source)
    )
}

/// [`inhabitant_datum`]'s own hand-map from [`hornvale_worldgen::energy::
/// EnergySource`] to a short descriptive phrase.
fn source_phrase(source: hornvale_worldgen::energy::EnergySource) -> &'static str {
    use hornvale_worldgen::energy::EnergySource;
    match source {
        EnergySource::Serpentinization => "the wet, serpentine rock",
        EnergySource::IronReduction => "iron-bearing stone",
        EnergySource::Radiolysis => "the rock's own faint radioactivity",
        EnergySource::SulphideOxidation => "a sulphide-laced seam",
        EnergySource::Methanogenesis => "the porous, water-logged carbonate",
        EnergySource::Geothermal => "the warmth rising from below",
        EnergySource::DetritalImport => "detritus washed down from above",
    }
}

/// The outcome of one lateral compass step underground (The Gallery, Task
/// 4; spec §3.2).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum StepOutcome {
    /// The possession moved one cell.
    Moved,
    /// The step was refused, with the physical reason why — never a parse
    /// complaint and never a sentence naming a verb or a movement mode.
    ///
    /// **Never constructed since `Underground::step`'s deletion** (the
    /// deleted all-in-one form was the only thing that built this variant
    /// from [`Underground::peek`]'s `Err`; [`Underground::commit_step`]'s
    /// own doc has always said it "never itself refuses"). Kept, not
    /// deleted, because `session.rs`'s `step_underground` still matches it
    /// as a real arm rather than `unreachable!()` — the same "fail loudly
    /// with a message, not a panic with no context" reasoning that arm's
    /// own doc gives for itself applies here: a future `commit_step` change
    /// that starts refusing has a variant ready to return.
    #[allow(dead_code)]
    Blocked(&'static str),
    /// The step landed on a stairs cell. Moving BETWEEN rungs needs the
    /// stairs verb (Task 5), not another compass bearing, so this is kept
    /// distinct from a plain [`StepOutcome::Moved`].
    NeedsStairs,
}

#[cfg(test)]
mod tests {
    use super::*;

    /// `rung_band` must name the rung `descent`'s own first element was
    /// generated for — [`Underground::enter`] always enters at rung `0`,
    /// which is `habitation_rungs()[0]`.
    #[test]
    fn rung_band_names_the_top_of_the_ladder() {
        assert_eq!(habitation_rungs()[0], Band::Undercroft);
    }

    /// `habitation_rungs` excludes `Surface` and keeps the ladder's order —
    /// the invariant [`Underground::rung`] relies on to index `descent`
    /// without drift.
    #[test]
    fn habitation_rungs_excludes_surface() {
        assert!(!habitation_rungs().contains(&Band::Surface));
        assert_eq!(habitation_rungs(), hornvale_terrain::rungs()[1..].to_vec());
    }

    // --- Task 6: fog of war (spec §3.5) -------------------------------

    /// `SeenBits::saw` reports exactly what `mark_all` has marked, and
    /// nothing else — the basic contract before monotonicity is asked
    /// about at all.
    #[test]
    fn saw_reports_exactly_what_was_marked() {
        let extent = Rect {
            x: 0,
            y: 0,
            w: 6,
            h: 4,
        };
        let mut seen = SeenBits::new(extent);
        let marked = Cell(2, 1);
        let unmarked = Cell(3, 1);
        assert!(!seen.saw(marked), "nothing is seen before marking");
        let mut cells = BTreeSet::new();
        cells.insert(marked);
        seen.mark_all(&cells);
        assert!(seen.saw(marked));
        assert!(!seen.saw(unmarked));
    }

    /// A cell outside the extent is never "seen" — `saw` tolerates it
    /// rather than panicking, the same tolerance `CellGrid::get` gives an
    /// out-of-bounds read.
    #[test]
    fn a_cell_outside_the_extent_is_never_seen() {
        let extent = Rect {
            x: 0,
            y: 0,
            w: 4,
            h: 4,
        };
        let seen = SeenBits::new(extent);
        assert!(!seen.saw(Cell(100, 100)));
        assert!(!seen.saw(Cell(-1, -1)));
    }

    /// Step 1's `a_wider_reach_does_not_change_what_was_already_seen`: mark
    /// a small set, then mark a larger, disjoint one — the small set's own
    /// bits must stay set. A bitset gets this for free; this says so out
    /// loud so a future refactor to a recomputed representation cannot
    /// quietly lose it (spec §3.5's "correct across a change in reach").
    #[test]
    fn a_wider_reach_does_not_change_what_was_already_seen() {
        let extent = Rect {
            x: 0,
            y: 0,
            w: 20,
            h: 20,
        };
        let mut seen = SeenBits::new(extent);
        let small: BTreeSet<Cell> = [(5, 5), (5, 6), (6, 5)]
            .into_iter()
            .map(|(x, y)| Cell(x, y))
            .collect();
        seen.mark_all(&small);
        for &c in &small {
            assert!(seen.saw(c), "must be seen right after marking: {c:?}");
        }

        let wide: BTreeSet<Cell> = (10..15)
            .flat_map(|x| (10..15).map(move |y| Cell(x, y)))
            .collect();
        seen.mark_all(&wide);

        for &c in &small {
            assert!(
                seen.saw(c),
                "a bit already set must stay set after marking a disjoint, \
                 wider set: {c:?}"
            );
        }
        for &c in &wide {
            assert!(seen.saw(c));
        }
    }

    /// A byte-level substring search — see
    /// `windows/vessel/tests/suite/affordance.rs`'s identical helper for
    /// why this operates on `&[u8]` rather than `&str`: the depth-tracking
    /// scan below slices at arbitrary byte offsets that need not land on a
    /// UTF-8 char boundary in a doc comment full of `—`/`§`/`×`.
    fn find_bytes(haystack: &[u8], needle: &[u8]) -> Option<usize> {
        if needle.is_empty() || haystack.len() < needle.len() {
            return None;
        }
        (0..=haystack.len() - needle.len()).find(|&i| &haystack[i..i + needle.len()] == needle)
    }

    /// Extracts the `{ ... }` block immediately following the first
    /// occurrence of `needle` in `src`, tracking brace depth from the
    /// block's own opening `{` to its matching close — the same helper
    /// `affordance.rs` uses to isolate one function's body out of a large
    /// file without also matching unrelated mentions elsewhere in it.
    fn block_body_after<'a>(src: &'a [u8], needle: &[u8]) -> Option<&'a [u8]> {
        let start = find_bytes(src, needle)? + needle.len();
        let open = start + src[start..].iter().position(|&b| b == b'{')?;
        let mut depth: i32 = 0;
        for (i, &b) in src[open..].iter().enumerate() {
            match b {
                b'{' => depth += 1,
                b'}' => {
                    depth -= 1;
                    if depth == 0 {
                        return Some(&src[open..=open + i]);
                    }
                }
                _ => {}
            }
        }
        None
    }

    /// Step 1's `the_reach_seam_is_the_only_source_of_the_radius`: a
    /// property, not a prescribed mutation. No literal sight radius appears
    /// anywhere on the underground sight path — the reach always comes
    /// through `Session::sight_reach()`. A structural source scan, because
    /// the property is about what the code does not contain, which no
    /// runtime assertion can witness
    /// (`affordance.rs`'s `no_verb_by_object_table_exists` is this repo's
    /// own precedent for the shape).
    ///
    /// Two checks: this module never spells `SIGHT_RADIUS` at all (nothing
    /// here has any other name for a sight distance to reach for), and
    /// `session.rs`'s own fog-marking function — the one call site that ORs
    /// a shadowcast into the rung bitset — reads the reach through
    /// `sight_reach()` rather than the constant directly. The positive
    /// control at the end guards against the scan silently matching nothing
    /// (a renamed function would make `block_body_after` return `None` and
    /// `expect` catches that; an emptied-out body would make the
    /// `sight_reach` check fail loudly instead of the whole test going
    /// vacuously green).
    #[test]
    fn the_reach_seam_is_the_only_source_of_the_radius() {
        // Scan only the PRODUCTION half of this file, split at the test
        // module's own opening attribute — this test's own assertion
        // strings (this one included) necessarily spell out the forbidden
        // name, and a scan of the whole file would trip on its own prose.
        let here = include_str!("underground.rs");
        let production = here
            .split("#[cfg(test)]")
            .next()
            .expect("split always yields at least one piece");
        assert!(
            !production.contains("SIGHT_RADIUS"),
            "underground.rs's production code must never name SIGHT_RADIUS \
             directly; the reach comes from Session::sight_reach()"
        );

        let session_src = include_str!("session.rs");
        // `block_body_after` finds the block's OWN opening `{` by scanning
        // forward from the needle's end — so the needle must stop short of
        // that brace (`affordance.rs`'s own doc-test example, `b"fn a()"`,
        // does the same), or the scan overshoots past it into the first
        // NESTED brace inside the body instead.
        let body = block_body_after(
            session_src.as_bytes(),
            b"fn mark_underground_seen(&mut self)",
        )
        .expect("session.rs must define fn mark_underground_seen(&mut self)");
        assert!(
            find_bytes(body, b"SIGHT_RADIUS").is_none(),
            "mark_underground_seen must read the reach through sight_reach(), \
             not SIGHT_RADIUS directly: {:?}",
            std::str::from_utf8(body).unwrap_or("<non-utf8>")
        );
        assert!(
            find_bytes(body, b"sight_reach").is_some(),
            "positive control: mark_underground_seen must actually call \
             sight_reach() somewhere in its body"
        );
    }

    // --- Task 11: inhabitants (spec §3.6) -----------------------------

    /// A real, unremarkable chamber: sea-level height, a mild temperature and
    /// a damp-but-not-flooded moisture reading — nothing authored to favour
    /// either subterranean species over the other. Only `height_asl_m` is
    /// varied by the tests below; `temperature_c`/`moisture` stay fixed here.
    fn ordinary_substrate(height_asl_m: f64) -> hornvale_worldgen::Substrate {
        hornvale_worldgen::Substrate {
            temperature_c: 5.0,
            moisture: 0.85,
            insolation: 0.0,
            height_asl_m: hornvale_kernel::SeaLevelHeight::from_metres(height_asl_m),
        }
    }

    /// **The property, not a fixed roster** (spec §3.6): perturbing either
    /// of `dominant_inhabitant`'s two inputs — the chamber's own energy
    /// reading, or its substrate — moves the result, and a chamber hostile
    /// enough on both axes at once holds nobody at all. No named species is
    /// asserted as a FIXED answer to a fixed input; each assertion compares
    /// two READINGS against each other or against `None`, which is what
    /// keeps this test from pinning a calibration Task 11 does not own.
    #[test]
    fn who_is_underground_derives_from_the_chambers_own_conditions() {
        let sea_level = ordinary_substrate(0.0);

        // Energy alone moves the roster, substrate held fixed. A starved
        // chamber favours rust-monster (untouched by energy — its niche
        // carries no CHEMOSYNTHATE weight); the same chamber, well-fed,
        // favours xorn instead (0.35 CHEMOSYNTHATE-weighted, THE SOURCES).
        let starved = dominant_inhabitant(&sea_level, 0.0);
        let fed = dominant_inhabitant(&sea_level, 1.0);
        assert!(
            starved.is_some() && fed.is_some(),
            "an ordinary chamber must hold somebody whether starved or fed: \
             starved={starved:?} fed={fed:?}"
        );
        assert_ne!(
            starved, fed,
            "starving vs feeding the SAME chamber must be able to change WHO \
             it holds, not merely whether: starved={starved:?} fed={fed:?}"
        );

        // Substrate alone moves the roster, energy held fixed at "starved"
        // (`0.0`) throughout: an ordinary chamber still holds rust-monster
        // (its own fit does not read energy at all), but a chamber whose
        // elevation sits far outside every authored niche's range holds
        // nobody — "a chamber that can feed nothing holds nothing" (spec
        // §3.6), demonstrated here through substrate alone rather than
        // energy. (Energy is held at `0.0`, not `1.0`, on purpose: a
        // well-fed xorn's energy term alone already clears
        // [`INHABITANT_FIT_THRESHOLD`] regardless of substrate, so pairing
        // an extreme substrate with full energy would not show substrate
        // moving anything — see [`inhabitant_fit`]'s own doc for why the
        // energy term is a floor a chemotroph can always reach when fed.)
        let extreme_height = ordinary_substrate(60_000.0);
        let hostile = dominant_inhabitant(&extreme_height, 0.0);
        assert_eq!(
            hostile, None,
            "moving ONLY the substrate (height 0 m -> 60,000 m), energy held \
             fixed at 0.0, must be able to empty a chamber that used to hold \
             somebody: ordinary={starved:?} extreme={hostile:?}"
        );
        assert_ne!(
            starved, hostile,
            "substrate alone must be able to move the roster"
        );
    }

    /// [`resident_cell`] picks the level's own LAST standable cell, the
    /// mirror of [`Underground::enter`]'s FIRST-cell entrance rule — and a
    /// fixed function of the level alone, not of any external state.
    #[test]
    fn resident_cell_is_the_levels_own_last_standable_cell() {
        let extent = Rect {
            x: 0,
            y: 0,
            w: 4,
            h: 3,
        };
        let mut cells = crate::underworld_level::CellGrid::new(extent, LevelCellKind::Wall);
        cells.set(Cell(1, 1), LevelCellKind::Floor);
        cells.set(Cell(2, 1), LevelCellKind::Flooded);
        let level = Level {
            extent,
            cells,
            dof: 0,
            leaf_styles: Vec::new(),
        };
        assert_eq!(resident_cell(&level), Some(Cell(2, 1)));
    }

    /// A level with no standable cell at all (degenerate, never produced by
    /// the real generator — Task 9's own connectivity invariant forbids it)
    /// has no resident cell either, rather than panicking.
    #[test]
    fn resident_cell_is_none_when_the_level_has_no_floor() {
        let extent = Rect {
            x: 0,
            y: 0,
            w: 3,
            h: 3,
        };
        let cells = crate::underworld_level::CellGrid::new(extent, LevelCellKind::Wall);
        let level = Level {
            extent,
            cells,
            dof: 0,
            leaf_styles: Vec::new(),
        };
        assert_eq!(resident_cell(&level), None);
    }

    /// [`inhabitant_datum`] names the species, not a personal label (this
    /// creature has no entity identity — see the function's own doc), and
    /// its flavour text changes with the dominant source even for the same
    /// species — a chamber whose dominant source differs reads as a
    /// genuinely different sentence, not a copy-pasted one.
    #[test]
    fn inhabitant_datum_names_the_species_and_the_source() {
        let a = inhabitant_datum(
            KindId("xorn"),
            hornvale_worldgen::energy::EnergySource::Geothermal,
        );
        let b = inhabitant_datum(
            KindId("xorn"),
            hornvale_worldgen::energy::EnergySource::DetritalImport,
        );
        assert!(a.contains("xorn") && b.contains("xorn"));
        assert_ne!(
            a, b,
            "a different dominant source must read differently: {a:?} vs {b:?}"
        );
    }
}
