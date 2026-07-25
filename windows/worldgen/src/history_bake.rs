//! The deep-history bake: the forward simulation at the heart of the
//! living-community engine. It seeds an ancient world with a handful of
//! proto-communities, steps epochs across paleoclimate era-variance, and
//! resolves grow / found / migrate / raid / flee / collapse / resettle into
//! an occupation skeleton (a `Vec<OccupationRecord>` — alive and dead).
//!
//! It lives at the composition root because it reads multiple domains
//! together (history's data model, paleoclimate's era masks, a demography
//! capacity field), which only worldgen may do (Constitution §2.6). A domain
//! may depend on nothing but the kernel, so the cross-domain bake cannot live
//! in `hornvale-history`; that crate documents the `history/*` seed labels the
//! bake draws under (`domains/history/src/streams.rs`).
//!
//! **The one invariant that matters most (measure-don't-narrate):**
//! conflict — communities raiding, fleeing and resettling — must GENUINELY
//! fire, at volume. Conflict here is PREDATION, not congestion (The Tumult):
//! a community raids a reachable neighbour whose land is worth more than its
//! own and whose strength it can beat, whether or not either of them is
//! crowded. Crowding governs only growth (logistic) and Famine (collapse at
//! `COLLAPSE_PRESSURE`); a hostile era mask evicts a community to a vacant
//! refuge or starves it, and never itself starts a fight. There is no minimum
//! anywhere in this file: if conflict comes out inert, the raid margin /
//! value gradient / seeding are wrong, not the measurement.
//!
//! Determinism: every arithmetic op stays in full `f64` precision (quantize
//! only at the emit boundary, which is Task 4 — not here); the genesis draws
//! derive per-people streams under `history/genesis/<people>`; the epoch
//! dynamics draw sequentially from one `history/bake` stream in commit order;
//! neighbour candidates sort by `f64::total_cmp`. Same seed ⇒ byte-identical
//! `records`.

use hornvale_history::record::{
    CauseOfEnd, Ended, Founding, Function, Notability, OccupationRecord, TechHorizon,
};
use hornvale_kernel::seed::StreamLabel;
use hornvale_kernel::{CellId, CellMap, EntityId, Geosphere, KindId, Seed, Stream};
use hornvale_paleoclimate::EraClimate;
use hornvale_topology::ConnectionGraph;
use std::collections::{BTreeMap, BTreeSet};

/// The conductance-positive graph neighbours of `cell` (`conductance > 0.0` —
/// ocean-touching adjacency edges are stored at exactly 0.0). Ascending and
/// deduplicated, matching `Geosphere::neighbors`' contract so the rerouted
/// BFS/scans stay deterministic. On an all-land graph this equals
/// `geo.neighbors(cell)`.
fn traversable_neighbors(graph: &ConnectionGraph, cell: CellId) -> Vec<CellId> {
    let mut ns: Vec<CellId> = graph
        .edges(cell)
        .iter()
        .filter(|e| e.conductance > 0.0)
        .map(|e| e.to)
        .collect();
    ns.sort();
    ns.dedup();
    ns
}

/// The nearest OCCUPIED cell to `from` (excluding `from`), by breadth-first
/// graph-hop distance over `graph`; within the nearest layer, lowest
/// `CellId` wins — a total, deterministic order (mirrors `nearest_dest`'s
/// BFS structure). Returns the occupying community's index (`node_index`'s
/// value), or `None` if no occupied cell is reachable. A free function (not
/// a `Bake` method) so it is unit-testable against a hand-built graph +
/// `node_index` without constructing a full `Bake`; [`Bake::nearest_occupied`]
/// delegates to it over the era graph in force.
fn nearest_occupied(
    graph: &ConnectionGraph,
    node_index: &BTreeMap<CellId, usize>,
    from: CellId,
) -> Option<usize> {
    let mut visited: BTreeSet<CellId> = BTreeSet::new();
    visited.insert(from);
    let mut frontier: Vec<CellId> = vec![from];
    while !frontier.is_empty() {
        let mut next: Vec<CellId> = Vec::new();
        let mut hits: Vec<(CellId, usize)> = Vec::new();
        for &c in &frontier {
            for n in traversable_neighbors(graph, c) {
                if visited.insert(n) {
                    next.push(n);
                    if let Some(&idx) = node_index.get(&n) {
                        hits.push((n, idx));
                    }
                }
            }
        }
        if !hits.is_empty() {
            hits.sort_by_key(|a| a.0); // lowest CellId in the nearest layer
            return Some(hits[0].1);
        }
        frontier = next;
    }
    None
}

/// Per-capita resource need. Pressure is `population * NEED / eff_capacity`;
/// kept an explicit constant so the pressure formula reads as the algorithm.
const NEED: f64 = 1.0;
/// Base per-epoch growth rate, damped logistically by `(1 - pressure)` so a
/// community asymptotes at its cell's effective capacity. Crowding is a growth
/// term only: it no longer starts fights (The Tumult), it merely decides how
/// big a community gets and, past `COLLAPSE_PRESSURE`, whether it starves.
const GROWTH_RATE: f64 = 0.2;
/// Fraction of a community's population that survives an orderly migration to
/// a new cell (the rest is lost on the journey).
const MIGRATE_SURVIVAL: f64 = 0.9;
/// How much stronger a raider must be than its target to attack (the dominance
/// margin). A save-format constant: changing it re-fights every world's
/// history.
const RAID_MARGIN: f64 = 1.5;
/// Fraction of population destroyed outright in a raid — applied to the raider
/// AND the loser alike, because spec §4.3's war is lossy in the *combined*
/// population: value leaves the system rather than being transferred. This is
/// the primary dissipation, and it is what makes a serial raider grind itself
/// down instead of snowballing.
const WAR_LOSS: f64 = 0.3;
/// Population below which a broken, displaced remnant dies out rather than
/// cascading further — the avalanche cutoff, and the second dissipation.
const VIABLE_MIN: f64 = 2.0;
/// Pressure at or above which a community starves out (Famine). `pub` so
/// the demography calibration (`windows/lab`) can express the aggregate
/// population-conservation ceiling: no live community exceeds this pressure,
/// so `Σ pop < COLLAPSE_PRESSURE × SETTLERS_PER_CAPACITY × Σ capacity`.
/// type-audit: bare-ok(ratio)
pub const COLLAPSE_PRESSURE: f64 = 2.0;
/// Pressure below which a comfortable community may throw off a daughter.
const DAUGHTER_MAX_PRESSURE: f64 = 0.7;
/// Per-epoch probability a comfortable community founds a daughter.
const DAUGHTER_PROB: f64 = 0.06;
/// Candidate cells (highest-capacity habitable of the earliest era) the
/// genesis seeding draws proto-sites from. Kept well above the total genesis
/// community count so every people finds its own vacant sites rather than
/// being starved by peoples seeded before it.
const GENESIS_TOP_CELLS: usize = 64;
/// Fewest / most proto-sites a single people seeds (drawn per people).
const GENESIS_SITES_MIN: u32 = 2;
/// Most proto-sites a single people seeds (inclusive upper bound of the draw).
const GENESIS_SITES_MAX: u32 = 4;
/// Starting population of a genesis proto-community.
const GENESIS_POP: f64 = 10.0;
/// Starting population of a daughter community.
const DAUGHTER_POP: f64 = 8.0;
/// How strongly river proximity sharpens site selection (Task 5b). Genesis
/// candidate ranking and daughter founding — the two paths that OPEN new
/// occupations — score a cell by `capacity * (1.0 + RIVER_SITE_WEIGHT *
/// river_proximity)` (proximity in `[0, 1]`, ~1 on a river, ~0 far from one),
/// so a river-adjacent cell outbids an equally-fertile one away from water.
///
/// This restores The Confluence's shipped property — settlements condensing
/// near fresh water — which the epoch (Task 5a) diluted: 5a's daughters
/// settled the first vacant neighbour (ignoring both capacity AND rivers) and
/// its migrations picked by refugia/CellId alone, so occupations spread off
/// the river network the base capacity spikes on. The dominant restoring
/// lever is re-ranking daughter founding by capacity at all: `capacity`
/// already folds the Confluence freshwater term (which rides
/// `river_proximity`), so preferring high-capacity neighbours pulls the
/// occupied set toward rivers on its own. `RIVER_SITE_WEIGHT` is the
/// additional, explicit river bias on top — a modest value already saturates
/// the near-river fraction because capacity carries most of the signal, so
/// this stays deliberately small. A save-format constant: changing it
/// re-places every world.
const RIVER_SITE_WEIGHT: f64 = 2.0;
/// Hard cap on a single relaxation cascade's BFS depth (displacement chain
/// length) — a safety bound against a pathological unbounded chain, not a
/// physical parameter. [`Bake::relocate`] reads it: a roll-downhill that
/// reaches this depth drops its last remnant rather than recursing further.
/// type-audit: bare-ok(count)
pub const CASCADE_DEPTH_CAP: u32 = 256;
/// Number of log2 bins in [`BakeCensus::cascade_hist`]: bin `i` counts
/// cascades whose size falls in `[2^i, 2^(i+1))`, covering sizes 1, 2, 3-4,
/// 5-8, … up to 2^11+.
const CASCADE_BINS: usize = 12;

/// Configuration for a deep-history bake: the span of years to simulate and
/// the epoch step. Years are bare `f64` (absolute, no wall-clock).
/// type-audit: bare-ok(count: start_year), bare-ok(count: end_year), bare-ok(count: epoch_years)
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct BakeConfig {
    /// The year the ancient world is seeded at (inclusive).
    pub start_year: f64,
    /// The year the bake closes at (`now`); alive records keep `ended = None`.
    pub end_year: f64,
    /// The step between epochs, in years.
    pub epoch_years: f64,
}

impl BakeConfig {
    /// The default bake span: two millennia in 25-year epochs.
    pub fn default_millennia() -> BakeConfig {
        BakeConfig {
            start_year: 0.0,
            end_year: 2000.0,
            epoch_years: 25.0,
        }
    }
}

/// The whole baked skeleton: every occupation record ever opened (alive and
/// dead), in deterministic commit order, plus the `now` the bake closed at.
/// The event census is tallied during the bake and read back by [`census`].
/// type-audit: bare-ok(count: now)
#[derive(Clone, Debug)]
pub struct History {
    /// Every occupation record, in commit (creation) order.
    pub records: Vec<OccupationRecord>,
    /// The standard-day/year the bake closed at.
    pub now: f64,
    /// Event tallies, counted as the bake resolves each epoch.
    tally: BakeCensus,
}

/// A tally of the events a bake resolved — the falsification instrument. Under
/// The Tumult's predation model the load-bearing counts are `raided`/`fled`
/// (conflict must fire on a *value* gradient, in worlds with land to spare, and
/// stay at zero in value-flat ones) read against `alive_at_now` (conquest must
/// redistribute the world, not depopulate it).
/// type-audit: bare-ok(count: grew), bare-ok(count: founded), bare-ok(count: migrated), bare-ok(count: raided), bare-ok(count: fled), bare-ok(count: collapsed), bare-ok(count: resettled), bare-ok(count: records_total), bare-ok(count: alive_at_now), bare-ok(count: cascade_hist)
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct BakeCensus {
    /// Grow events (a community expanded under a sub-capacity load).
    pub grew: u64,
    /// Daughter-founding events (a comfortable community spawned a daughter).
    pub founded: u64,
    /// Migration events (a community relocated off a cell turned hostile).
    pub migrated: u64,
    /// Raid events (a community conquered a weaker neighbour's better land,
    /// moving onto the seized site).
    pub raided: u64,
    /// Flee events (a raided community abandoned its site).
    pub fled: u64,
    /// Collapse events (a community starved out — Famine).
    pub collapsed: u64,
    /// Resettle events (a fled community refounded on a vacant habitable cell).
    pub resettled: u64,
    /// Total records opened over the whole bake.
    pub records_total: u64,
    /// Records still alive at `now`.
    pub alive_at_now: u64,
    /// Log-binned histogram of cascade sizes (# displacements in one
    /// relaxation): bin i counts cascades whose size falls in
    /// `[2^i, 2^(i+1))`. The raw material of The Tumult's power-law
    /// falsification metric. Not committed to save format (diagnostic only).
    pub cascade_hist: [u64; CASCADE_BINS],
}

impl BakeCensus {
    /// Record one completed cascade of `size` displacements into the
    /// log-binned histogram. `size == 0` (a relocation that reached vacant
    /// land directly, not a cascade) is not recorded.
    fn record_cascade(&mut self, size: u32) {
        if size == 0 {
            return;
        }
        let bin = (31 - size.leading_zeros()).min(CASCADE_BINS as u32 - 1) as usize;
        self.cascade_hist[bin] += 1;
    }
}

/// Read the event census off a baked history.
pub fn census(h: &History) -> BakeCensus {
    h.tally
}

/// The cascade-size histogram off a baked history (bin `i` = sizes
/// `[2^i, 2^(i+1))`). Filled by [`BakeCensus::record_cascade`], which
/// [`Bake::maybe_raid`] calls for every raid whose displaced loser had to
/// evict someone in turn. It stays all-zero on an unsaturated world — there,
/// every remnant finds vacant land at the first hop, so no relaxation chains —
/// which is a measurement of the branching ratio, not missing scaffolding.
/// type-audit: bare-ok(count: return)
pub fn cascade_sizes(h: &History) -> [u64; CASCADE_BINS] {
    h.tally.cascade_hist
}

impl History {
    /// Build a `History` directly from a record list and its `now` day — the
    /// hand-built constructor Task 4's tests (and any other non-bake
    /// producer) use to reach the private `tally` field from outside this
    /// module. The tally starts at its default (zero): a hand-built history
    /// was never actually baked, so there is no genuine event count to report.
    /// type-audit: bare-ok(count: now)
    pub fn new(records: Vec<OccupationRecord>, now: f64) -> History {
        History {
            records,
            now,
            tally: BakeCensus::default(),
        }
    }
}

/// One alive (or lately-dead) community's live state during the bake. The
/// `record` index ties it to its `OccupationRecord`; population is carried in
/// full `f64` precision.
struct Community {
    /// Index into `Bake::records` of this community's occupation record.
    record: usize,
    /// The cell this community currently occupies.
    site: CellId,
    /// The community's own entity handle.
    id: EntityId,
    /// The lineage this community continues (inherited by daughters/refounds).
    lineage: EntityId,
    /// Current population (full precision).
    population: f64,
    /// Whether the community is still alive.
    alive: bool,
    /// The tech horizon reached (monotone).
    tech: TechHorizon,
    /// Per-people tech-advance offset (years), drawn at genesis.
    tech_offset: f64,
}

/// The mutable bake state: records, live communities, the one-alive-per-site
/// index, the id counter, the dynamics stream, and the running tally.
struct Bake<'a> {
    /// One era-aware connection graph per era (`graphs.len() == eras.len()`).
    /// Each graph's traversable neighbours (`conductance > 0.0`) are that era's
    /// passable geography: the glacial low-stand exposes shelf land bridges, the
    /// rising sea drowns them. The bake walks `cur()` — the era being stepped.
    graphs: &'a [ConnectionGraph],
    /// Index into `graphs` of the era currently being stepped.
    cur_graph: usize,
    /// Per-cell base carrying capacity.
    capacity: &'a CellMap<f64>,
    /// Per-cell proximity to fresh flowing water in `[0, 1]` (~1 on a river,
    /// ~0 far from one). Biases all three site-picking paths toward water so
    /// settlements condense near rivers (Task 5b, restoring The Confluence).
    river_prox: &'a CellMap<f64>,
    /// Cells habitable through the glacial maximum (migration preference).
    refugia: &'a CellMap<bool>,
    /// Every occupation record, in commit order.
    records: Vec<OccupationRecord>,
    /// Every community's live state, in commit order (dead ones retained).
    communities: Vec<Community>,
    /// The single alive community per occupied cell (the scan≡index invariant).
    node_index: BTreeMap<CellId, usize>,
    /// Next entity id to mint (never reused).
    next_id: u64,
    /// The epoch-dynamics random stream (drawn sequentially in commit order).
    stream: Stream,
    /// The running event tally.
    tally: BakeCensus,
}

/// The outcome of [`Bake::relocate`]ing a homeless people.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Relocation {
    /// Found a home (possibly by displacing occupants); `cascade` = the number
    /// of occupied cells this relocation displaced (0 = reached vacant land).
    Settled {
        /// Occupied cells displaced to reach this home (0 = vacant land).
        cascade: u32,
    },
    /// Vanished — no vacant cell and no occupied cell reachable (an isolated
    /// remnant), or truncated at the depth cap.
    Lost,
}

/// The river-proximity suitability multiplier for a cell (Task 5b): a cell on
/// a river (`prox` ≈ 1) is `1.0 + RIVER_SITE_WEIGHT` times as attractive as one
/// far from water (`prox` ≈ 0). Full precision; used to bias all three
/// site-picking paths toward fresh water.
fn river_factor(prox: f64) -> f64 {
    1.0 + RIVER_SITE_WEIGHT * prox
}

/// The tech horizon for an (offset-adjusted) absolute `year`. Callers pass
/// `year + per-people offset`; monotone in `year`, so tech only ever rises.
fn tech_for(year: f64) -> TechHorizon {
    if year < 400.0 {
        TechHorizon::Neolithic
    } else if year < 900.0 {
        TechHorizon::Bronze
    } else if year < 1400.0 {
        TechHorizon::Iron
    } else {
        TechHorizon::Classical
    }
}

/// The tech multiplier on raw population when reckoning a community's raiding
/// strength — Iron beats Bronze beats Neolithic. Monotone in `TechHorizon`.
fn tech_weight(t: TechHorizon) -> f64 {
    match t {
        TechHorizon::Neolithic => 1.0,
        TechHorizon::Bronze => 1.5,
        TechHorizon::Iron => 2.25,
        TechHorizon::Classical => 3.0,
    }
}

impl<'a> Bake<'a> {
    /// Mint a fresh, never-reused entity id.
    fn mint(&mut self) -> EntityId {
        let id = EntityId::new(self.next_id).expect("entity ids start at 1");
        self.next_id += 1;
        id
    }

    /// The graph for the era currently being stepped.
    fn cur(&self) -> &ConnectionGraph {
        &self.graphs[self.cur_graph]
    }

    /// The index of the era in force for `year`: the last era whose `day` is at
    /// or before `year`, or 0 for years before the first.
    fn era_index_for(&self, eras: &[EraClimate], year: f64) -> usize {
        let mut chosen = 0;
        for (i, e) in eras.iter().enumerate() {
            if e.day <= year {
                chosen = i;
            }
        }
        chosen
    }

    /// A cell's habitability factor this era: 1.0 if habitable and unglaciated,
    /// else 0.0. (Binary here; the swing, not a gradient, drives the dynamics.)
    fn factor(era: &EraClimate, cell: CellId) -> f64 {
        if *era.ice.get(cell) || !*era.habitable.get(cell) {
            0.0
        } else {
            1.0
        }
    }

    /// A cell's effective capacity this era.
    fn eff_capacity(&self, era: &EraClimate, cell: CellId) -> f64 {
        *self.capacity.get(cell) * Self::factor(era, cell)
    }

    /// Whether a cell can receive a settler this era: habitable, unglaciated,
    /// and not already occupied by an alive community.
    fn vacant_habitable(&self, era: &EraClimate, cell: CellId) -> bool {
        Self::factor(era, cell) > 0.0 && !self.node_index.contains_key(&cell)
    }

    /// The nearest vacant habitable cell to `from` (excluding `from` itself),
    /// by breadth-first hop distance. Within the nearest layer, refugial cells
    /// win over non-refugial, then lowest `CellId` — a total, deterministic
    /// order. `None` if the whole reachable graph is full or hostile.
    fn nearest_dest(&self, era: &EraClimate, from: CellId) -> Option<CellId> {
        let mut visited: BTreeSet<CellId> = BTreeSet::new();
        visited.insert(from);
        let mut frontier: Vec<CellId> = vec![from];
        while !frontier.is_empty() {
            let mut next: Vec<CellId> = Vec::new();
            let mut candidates: Vec<CellId> = Vec::new();
            for &c in &frontier {
                for n in traversable_neighbors(self.cur(), c) {
                    if visited.insert(n) {
                        next.push(n);
                        if self.vacant_habitable(era, n) {
                            candidates.push(n);
                        }
                    }
                }
            }
            if !candidates.is_empty() {
                // Refugial first (survival through the glacial maximum is the
                // point of a migration), then river-adjacent as a tie-break
                // (Task 5b — bias toward water among otherwise-equal refuges),
                // then lowest CellId — total & deterministic (`f64::total_cmp`).
                candidates.sort_by(|a, b| {
                    let ra = *self.refugia.get(*a);
                    let rb = *self.refugia.get(*b);
                    let pa = *self.river_prox.get(*a);
                    let pb = *self.river_prox.get(*b);
                    rb.cmp(&ra).then(pb.total_cmp(&pa)).then(a.cmp(b))
                });
                return Some(candidates[0]);
            }
            frontier = next;
        }
        None
    }

    /// The nearest OCCUPIED cell to `from` (excluding `from`), over the era
    /// graph currently being stepped. Delegates to the free [`nearest_occupied`]
    /// function so the BFS/tie-break logic is unit-testable against a
    /// hand-built graph + `node_index` without constructing a full `Bake`.
    /// [`Bake::relocate`] displaces this cell's occupant when no vacant land is
    /// reachable — it is the roll-downhill's next victim, and the step that
    /// turns a single raid into a chained cascade.
    fn nearest_occupied(&self, from: CellId) -> Option<usize> {
        nearest_occupied(self.cur(), &self.node_index, from)
    }

    /// Relocate a homeless people (a remnant driven off its land by a raid) to
    /// a new home, cascading when there is no vacant land. `predecessor` is the
    /// id of the community that just closed and is relocating (used to
    /// attribute the new occupation's `Founding::From` to its specific
    /// forebear, not the lineage ancestor — `lineage` stays reserved for the
    /// `open` lineage argument). Returns the outcome: [`Relocation::Settled`]
    /// (with the cascade size — the number of OCCUPIED cells this relocation
    /// displaced, 0 if it reached vacant land directly) or
    /// [`Relocation::Lost`] (the remnant died, nothing was reachable, or the
    /// depth cap truncated the chain). The roll-downhill: no vacant cell ⇒
    /// take the nearest occupied cell, and its evicted occupant relocates in
    /// turn. Bounded twice — by `VIABLE_MIN` (a remnant too small to hold any
    /// land dies out instead of founding a peopleless occupation: the
    /// dissipation of spec §4.3) and by `CASCADE_DEPTH_CAP` (a truncated
    /// cascade drops the last remnant).
    #[allow(clippy::too_many_arguments)]
    fn relocate(
        &mut self,
        people: KindId,
        pop: f64,
        lineage: EntityId,
        predecessor: EntityId,
        offset: f64,
        from: CellId,
        era: &EraClimate,
        year: f64,
        depth: u32,
    ) -> Relocation {
        if depth >= CASCADE_DEPTH_CAP {
            return Relocation::Lost; // truncated — the last remnant is lost (bounded-size guard)
        }
        if pop < VIABLE_MIN {
            // The remnant is too small to hold land anywhere: it dies out
            // rather than founding a peopleless occupation (spec §4.3's
            // viable-minimum death — the second dissipation).
            return Relocation::Lost;
        }
        // Vacant land reachable? Then no conflict — settle there.
        if let Some(dest) = self.nearest_dest(era, from) {
            let new_idx = self.open(
                people,
                dest,
                year,
                pop,
                Founding::From(predecessor),
                Some(lineage),
                offset,
            );
            self.touch(new_idx, year);
            return Relocation::Settled { cascade: 0 };
        }
        // No vacant land — displace the nearest occupied cell (the avalanche).
        let Some(victim) = self.nearest_occupied(from) else {
            return Relocation::Lost; // nothing vacant AND nothing occupied reachable — lost
        };
        let victim_site = self.communities[victim].site;
        let (v_people, v_pop, v_lineage, v_offset, v_id) = {
            let c = &self.communities[victim];
            (
                self.records[c.record].people,
                c.population,
                c.lineage,
                c.tech_offset,
                c.id,
            )
        };
        // The homeless people takes the victim's site (open BEFORE close so
        // node_index[victim_site] points at the new occupant; close then sees
        // the cell already re-indexed and does not free it).
        let new_idx = self.open(
            people,
            victim_site,
            year,
            pop,
            Founding::From(predecessor),
            Some(lineage),
            offset,
        );
        let displacer_id = self.communities[new_idx].id;
        self.close(victim, year, CauseOfEnd::Fled, Ended::By(displacer_id));
        self.touch(new_idx, year);
        self.tally.raided += 1;
        self.tally.fled += 1;
        // The evicted occupant cascades onward, founded from its own (the
        // victim's) community id.
        let victim_cascade = match self.relocate(
            v_people,
            v_pop * MIGRATE_SURVIVAL,
            v_lineage,
            v_id,
            v_offset,
            victim_site,
            era,
            year,
            depth + 1,
        ) {
            Relocation::Settled { cascade } => cascade,
            Relocation::Lost => 0,
        };
        Relocation::Settled {
            cascade: 1 + victim_cascade,
        }
    }

    /// A community's raiding strength: its population scaled by its tech
    /// horizon. Heterogeneous strength is the fuel of predation — equals do
    /// not prey on one another.
    fn strength(&self, idx: usize) -> f64 {
        let c = &self.communities[idx];
        c.population * tech_weight(c.tech)
    }

    /// Open a new occupation record + live community at `site`, and return the
    /// community's index. Registers it in the one-alive-per-site index.
    #[allow(clippy::too_many_arguments)]
    fn open(
        &mut self,
        people: KindId,
        site: CellId,
        year: f64,
        population: f64,
        founded_from: Founding,
        lineage: Option<EntityId>,
        tech_offset: f64,
    ) -> usize {
        let id = self.mint();
        let lineage = lineage.unwrap_or(id);
        let tech = tech_for(year + tech_offset);
        let record = OccupationRecord {
            people,
            community: id,
            lineage,
            site,
            founded: year,
            ended: None,
            peak_population: population.round() as u32,
            tech,
            function: Function::Agrarian,
            deity: None,
            tongue: None,
            cause: None,
            ended_by: Ended::Nature,
            founded_from,
            notability: Notability::Common,
        };
        let record_idx = self.records.len();
        self.records.push(record);
        let community_idx = self.communities.len();
        self.communities.push(Community {
            record: record_idx,
            site,
            id,
            lineage,
            population,
            alive: true,
            tech,
            tech_offset,
        });
        self.node_index.insert(site, community_idx);
        self.tally.records_total += 1;
        community_idx
    }

    /// Close a community's record: mark it dead, stamp the ending, and free its
    /// cell from the one-alive-per-site index.
    fn close(&mut self, idx: usize, year: f64, cause: CauseOfEnd, ended_by: Ended) {
        let c = &mut self.communities[idx];
        c.alive = false;
        let site = c.site;
        let rec = c.record;
        self.records[rec].ended = Some(year);
        self.records[rec].cause = Some(cause);
        self.records[rec].ended_by = ended_by;
        // Only free the cell if THIS community is the one indexed there.
        if self.node_index.get(&site) == Some(&idx) {
            self.node_index.remove(&site);
        }
    }

    /// Update a community's peak population and monotone tech from its current
    /// state at `year`.
    fn touch(&mut self, idx: usize, year: f64) {
        let c = &mut self.communities[idx];
        let peak = c.population.round() as u32;
        let rec = c.record;
        if peak > self.records[rec].peak_population {
            self.records[rec].peak_population = peak;
        }
        let tech = tech_for(year + c.tech_offset);
        if tech > c.tech {
            c.tech = tech;
        }
        if c.tech > self.records[rec].tech {
            self.records[rec].tech = c.tech;
        }
    }

    /// Resolve one community for one epoch (migrate / collapse / grow / raid).
    /// Newly opened communities are processed the following epoch.
    ///
    /// Crowding is not a conflict trigger (The Tumult): it feeds the logistic
    /// growth term and, past `COLLAPSE_PRESSURE`, starves the community. A
    /// cell turned hostile evicts its community to a vacant refuge or kills
    /// it — a climate eviction never starts a fight, and never cascades.
    fn step_community(&mut self, idx: usize, era: &EraClimate, year: f64) {
        if !self.communities[idx].alive {
            return;
        }
        let site = self.communities[idx].site;
        let eff = self.eff_capacity(era, site);

        // Climate eviction: migrate to a vacant refuge, or starve. No conflict.
        if eff == 0.0 {
            let (record, pop, lineage, offset, migrant_id) = {
                let c = &self.communities[idx];
                (c.record, c.population, c.lineage, c.tech_offset, c.id)
            };
            let people = self.records[record].people;
            // A refuge is only a refuge for a band big enough to hold it: a
            // migrant whose arriving population would fall below `VIABLE_MIN`
            // starves on the road instead of refounding, exactly as `relocate`
            // rules for a displaced remnant. Without this the refound would
            // `open` at `peak_population == 0` — a peopleless settlement.
            match self.nearest_dest(era, site) {
                Some(dest) if pop * MIGRATE_SURVIVAL >= VIABLE_MIN => {
                    self.close(idx, year, CauseOfEnd::Migrated, Ended::Nature);
                    let new_idx = self.open(
                        people,
                        dest,
                        year,
                        pop * MIGRATE_SURVIVAL,
                        Founding::From(migrant_id),
                        Some(lineage),
                        offset,
                    );
                    self.touch(new_idx, year);
                    self.tally.migrated += 1;
                }
                _ => {
                    self.close(idx, year, CauseOfEnd::Famine, Ended::Nature);
                    self.tally.collapsed += 1;
                }
            }
            return;
        }

        let pressure = self.communities[idx].population * NEED / eff;

        if pressure >= COLLAPSE_PRESSURE {
            self.close(idx, year, CauseOfEnd::Famine, Ended::Nature);
            self.tally.collapsed += 1;
            return;
        }

        self.grow(idx, era, year, pressure);

        // Opportunistic predation — decoupled from this community's own
        // crowding (density is NOT the trigger).
        if self.communities[idx].alive {
            self.maybe_raid(idx, era, year);
        }
    }

    /// Opportunistic predation (The Tumult): a community raids the reachable
    /// occupied neighbour whose land is worth MORE than its own **this era**
    /// (covetousness) and whose strength it can beat by `RAID_MARGIN`
    /// (dominance) — decoupled from its own crowding.
    ///
    /// Value is [`Bake::eff_capacity`], never raw capacity: what a cell is
    /// worth to a conqueror is what it will actually yield under this era's
    /// ice/habitability mask. Reading raw capacity would let a community on
    /// good land covet a neighbour whose cell just turned hostile — abandoning
    /// a living site for ground worth nothing, preferentially at exactly the
    /// era-mask flips displacement is measured on. Since `maybe_raid` only runs
    /// for a raider whose own effective capacity is non-zero, requiring the
    /// prize to be strictly *more* valuable also excludes every zero-factor
    /// cell for free.
    ///
    /// The outcome is **conquest of immobile land**, not plunder (spec §4.3).
    /// The prize is the cell: war destroys `WAR_LOSS` of *each* side's
    /// population, the raider abandons its own poorer site (`Migrated`,
    /// `Ended::Nature` — an orderly, self-directed move) and reopens on the
    /// seized cell, and the loser is driven off on EVERY raid (`Fled`,
    /// `ended_by = By(raider)`), rolling downhill via [`Bake::relocate`] with
    /// whatever strength it has left. Taking *people* would be captives, an
    /// explicit spec §9 non-goal. The raider's old cell falls vacant — it left
    /// its poor land for the prize — and is therefore itself a candidate
    /// refuge for the remnant it just displaced.
    ///
    /// A raider whose post-war population would fall below `VIABLE_MIN`
    /// declines the fight: dominance is a *ratio*, so nothing else bounds a
    /// raider's absolute size, and a sub-viable seat would `open` with
    /// `peak_population == 0` — a peopleless settlement, which the shipped
    /// no-peopleless-settlements invariant forbids.
    ///
    /// Deterministic and draw-free: it picks the most valuable such target,
    /// tie-broken by the weakest, then the lowest `CellId` (`f64::total_cmp`
    /// throughout), and never touches the epoch stream — `maybe_raid` itself
    /// consumes no draw. It does change which communities exist and how
    /// pressured they are, so the *sequence* of `grow`'s `DAUGHTER_PROB` draws
    /// downstream does move; that is the genesis epoch spec §7 declares, not a
    /// break in byte-identity for a fixed physics.
    fn maybe_raid(&mut self, raider: usize, era: &EraClimate, year: f64) {
        let raider_site = self.communities[raider].site;
        // Too small to seat itself after the war it is contemplating: decline,
        // before any tally moves (see the `VIABLE_MIN` note above).
        if self.communities[raider].population * (1.0 - WAR_LOSS) < VIABLE_MIN {
            return;
        }
        let raider_str = self.strength(raider);
        let raider_val = self.eff_capacity(era, raider_site);
        // (target index, that cell's value, the target's strength, its cell)
        let mut best: Option<(usize, f64, f64, CellId)> = None;
        for n in traversable_neighbors(self.cur(), raider_site) {
            let Some(&t) = self.node_index.get(&n) else {
                continue;
            };
            let t_val = self.eff_capacity(era, n);
            let t_str = self.strength(t);
            if t_val <= raider_val {
                continue; // covet only land that is BETTER *this era*
            }
            if raider_str <= t_str * RAID_MARGIN {
                continue; // dominance: only a fight it can win
            }
            let better = match best {
                None => true,
                Some((_, bv, bs, bc)) => t_val
                    .total_cmp(&bv) // the MOST valuable land
                    .then(bs.total_cmp(&t_str)) // among equal value, the WEAKEST
                    .then(bc.cmp(&n)) // then the lowest CellId
                    .is_gt(),
            };
            if better {
                best = Some((t, t_val, t_str, n));
            }
        }
        let Some((target, _, _, _)) = best else {
            return; // nothing worth taking, or nothing beatable
        };

        self.tally.raided += 1;
        let prize = self.communities[target].site;
        let (raider_people, raider_id, raider_lineage, raider_offset) = {
            let c = &self.communities[raider];
            (
                self.records[c.record].people,
                c.id,
                c.lineage,
                c.tech_offset,
            )
        };
        let (loser_people, loser_id, loser_lineage, loser_offset) = {
            let c = &self.communities[target];
            (
                self.records[c.record].people,
                c.id,
                c.lineage,
                c.tech_offset,
            )
        };

        // War is lossy on BOTH sides: a fraction of the combined population is
        // destroyed in the taking, not transferred (spec §4.3).
        self.communities[raider].population *= 1.0 - WAR_LOSS;
        self.communities[target].population *= 1.0 - WAR_LOSS;
        let raider_pop = self.communities[raider].population;
        let loser_pop = self.communities[target].population;

        // Sequence the index bookkeeping so the one-alive-per-site invariant
        // holds at every step and no cell ever points at a dead community.
        // `close` frees a cell only when the closing community is the one
        // indexed there, so closing BOTH sides first leaves the raider's old
        // cell vacant (it left) and the prize vacant, and the `open` that
        // follows re-indexes the prize onto its new, living occupant.
        self.close(raider, year, CauseOfEnd::Migrated, Ended::Nature);
        self.close(target, year, CauseOfEnd::Fled, Ended::By(raider_id));
        self.tally.fled += 1;
        let seat = self.open(
            raider_people,
            prize,
            year,
            raider_pop,
            Founding::From(raider_id),
            Some(raider_lineage),
            raider_offset,
        );
        self.touch(seat, year);

        // The displaced loser rolls downhill, still carrying its (reduced)
        // strength — the cascade. Its own former site is now the raider's, so
        // it relocates away from `prize`.
        match self.relocate(
            loser_people,
            loser_pop,
            loser_lineage,
            loser_id,
            loser_offset,
            prize,
            era,
            year,
            0,
        ) {
            Relocation::Settled { cascade: 0 } => self.tally.resettled += 1,
            Relocation::Settled { cascade } => self.tally.record_cascade(cascade),
            Relocation::Lost => self.tally.collapsed += 1,
        }
    }

    /// A comfortable community grows logistically, and — if very comfortable —
    /// may throw off a daughter onto a vacant habitable neighbour.
    fn grow(&mut self, idx: usize, era: &EraClimate, year: f64, pressure: f64) {
        let c = &mut self.communities[idx];
        c.population *= 1.0 + GROWTH_RATE * (1.0 - pressure);
        self.touch(idx, year);
        self.tally.grew += 1;

        if pressure < DAUGHTER_MAX_PRESSURE && self.stream.next_f64() < DAUGHTER_PROB {
            let site = self.communities[idx].site;
            // A daughter settles the vacant habitable direct neighbour of
            // highest river-weighted capacity (Task 5b) — the dominant source
            // of new settlements, so this is the main lever that pulls the
            // occupied set toward fresh water. `RIVER_SITE_WEIGHT` tunes how
            // hard river proximity outbids raw capacity here. Tie-broken by
            // lowest CellId — total & deterministic (`f64::total_cmp`).
            let dest = traversable_neighbors(self.cur(), site)
                .into_iter()
                .filter(|&n| self.vacant_habitable(era, n))
                .max_by(|a, b| {
                    let sa = *self.capacity.get(*a) * river_factor(*self.river_prox.get(*a));
                    let sb = *self.capacity.get(*b) * river_factor(*self.river_prox.get(*b));
                    // Higher score wins; among equal score, lower CellId wins
                    // (treated as "greater" for `max_by`).
                    sa.total_cmp(&sb).then(b.cmp(a))
                });
            if let Some(dest) = dest {
                let (people, lineage, offset) = {
                    let c = &self.communities[idx];
                    (self.records[c.record].people, c.lineage, c.tech_offset)
                };
                let new_idx = self.open(
                    people,
                    dest,
                    year,
                    DAUGHTER_POP,
                    Founding::From(self.communities[idx].id),
                    Some(lineage),
                    offset,
                );
                self.touch(new_idx, year);
                self.tally.founded += 1;
            }
        }
    }
}

/// Seed the ancient world with proto-communities and step the epochs across
/// the paleoclimate era-variance, resolving the whole occupation skeleton.
///
/// See the module docs for the determinism contract and the
/// displacement-fires invariant.
/// type-audit: bare-ok(count: capacity), bare-ok(ratio: river_prox), bare-ok(flag: refugia)
// The bake reads several independent composition-root fields (geo, capacity,
// river proximity, era series, refugia, roster, span); each is a distinct
// world input with no coherent grouping into a single struct, so they stay
// explicit arguments (Task 5b added `river_prox`).
#[allow(clippy::too_many_arguments)]
pub fn bake(
    seed: Seed,
    geo: &Geosphere,
    capacity: &CellMap<f64>,
    river_prox: &CellMap<f64>,
    eras: &[EraClimate],
    refugia: &CellMap<bool>,
    peoples: &[KindId],
    cfg: &BakeConfig,
    graphs: &[ConnectionGraph],
) -> History {
    assert_eq!(graphs.len(), eras.len(), "one graph per era");
    let mut bake = Bake {
        graphs,
        cur_graph: 0,
        capacity,
        river_prox,
        refugia,
        records: Vec::new(),
        communities: Vec::new(),
        node_index: BTreeMap::new(),
        next_id: 1,
        stream: seed.derive(hornvale_history::streams::BAKE).stream(),
        tally: BakeCensus::default(),
    };

    // 1. Seed the ancient world at the earliest era's habitable, highest-
    //    capacity cells — one alive community per site. The candidate pool
    //    (`GENESIS_TOP_CELLS`) is kept well above the total genesis community
    //    count so EVERY people finds its own vacant proto-sites: a small
    //    shared pool would let the peoples processed first take every cell and
    //    starve the rest (a world missing a whole people). Each people draws
    //    from the cells still vacant when its turn comes, retrying past a
    //    collision rather than wasting the draw, so its `count` sites really
    //    do open (as long as vacant top cells remain).
    let earliest = eras
        .iter()
        .min_by(|a, b| a.day.total_cmp(&b.day))
        .expect("at least one era");
    let mut candidates: Vec<CellId> = geo
        .cells()
        .filter(|&c| Bake::factor(earliest, c) > 0.0)
        .collect();
    // Rank candidate proto-sites by river-weighted capacity (Task 5b): a
    // river-adjacent cell outranks an equally-fertile cell far from water, so
    // the genesis pool — and thus the peoples seeded from it — cluster near
    // fresh water. Tie-broken by lowest CellId (total, deterministic).
    candidates.sort_by(|a, b| {
        let sa = *capacity.get(*a) * river_factor(*river_prox.get(*a));
        let sb = *capacity.get(*b) * river_factor(*river_prox.get(*b));
        sb.total_cmp(&sa).then(a.cmp(b))
    });
    let top: Vec<CellId> = candidates.iter().copied().take(GENESIS_TOP_CELLS).collect();

    for &people in peoples {
        let mut pstream = seed
            .derive(hornvale_history::streams::GENESIS)
            .derive(StreamLabel::dynamic(people.0))
            .stream();
        let count = pstream.range_u32(GENESIS_SITES_MIN, GENESIS_SITES_MAX);
        // Only cells still vacant this people's turn are candidates — prior
        // peoples' proto-sites are excluded up front, so no draw is wasted.
        let mut pool: Vec<CellId> = top
            .iter()
            .copied()
            .filter(|c| !bake.node_index.contains_key(c))
            .collect();
        let mut opened = 0;
        while opened < count && !pool.is_empty() {
            let pick = pstream.range_u32(0, pool.len() as u32 - 1) as usize;
            let site = pool.swap_remove(pick);
            let offset = pstream.range_u32(0, 300) as f64;
            bake.open(
                people,
                site,
                cfg.start_year,
                GENESIS_POP,
                Founding::Genesis(site),
                None,
                offset,
            );
            opened += 1;
        }
    }

    // 2. Step epochs. Snapshot the alive set at the start of each epoch so a
    //    newly opened community is processed the following epoch (and the
    //    stream-draw order stays deterministic).
    let mut year = cfg.start_year;
    while year < cfg.end_year {
        let era_idx = bake.era_index_for(eras, year);
        bake.cur_graph = era_idx;
        let era = eras[era_idx].clone();
        let snapshot: Vec<usize> = (0..bake.communities.len())
            .filter(|&i| bake.communities[i].alive)
            .collect();
        for idx in snapshot {
            bake.step_community(idx, &era, year);
        }
        year += cfg.epoch_years;
    }

    // 3. Close at `now`: alive records keep `ended = None`.
    let now = cfg.end_year;
    bake.tally.alive_at_now = bake.records.iter().filter(|r| r.is_alive()).count() as u64;

    History {
        records: bake.records,
        now,
        tally: bake.tally,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_topology::{ConnectionGraph, Edge, EdgeKind};

    #[test]
    fn traversable_neighbors_excludes_ocean_includes_lanes() {
        let mut g = ConnectionGraph::new(4);
        g.add_edge(
            CellId(0),
            Edge {
                to: CellId(1),
                kind: EdgeKind::Adjacency,
                conductance: 1.0,
            },
        );
        g.add_edge(
            CellId(1),
            Edge {
                to: CellId(2),
                kind: EdgeKind::Adjacency,
                conductance: 0.0,
            },
        );
        g.add_edge(
            CellId(1),
            Edge {
                to: CellId(3),
                kind: EdgeKind::WaterRoute,
                conductance: 0.5,
            },
        );
        assert_eq!(
            traversable_neighbors(&g, CellId(1)),
            vec![CellId(0), CellId(3)]
        );
    }

    #[test]
    fn traversable_neighbors_dedups_parallel_edges() {
        let mut g = ConnectionGraph::new(2);
        g.add_edge(
            CellId(0),
            Edge {
                to: CellId(1),
                kind: EdgeKind::Adjacency,
                conductance: 1.0,
            },
        );
        g.add_edge(
            CellId(0),
            Edge {
                to: CellId(1),
                kind: EdgeKind::WaterRoute,
                conductance: 0.5,
            },
        );
        assert_eq!(traversable_neighbors(&g, CellId(0)), vec![CellId(1)]);
    }

    /// A pure-land connection graph over `geo` (unit-conductance adjacency,
    /// no water routes) — mirrors the integration test file's `full_land_graph`
    /// helper, duplicated here because `Bake` (and this free function) are
    /// private to this module and the unit test can't reach the integration
    /// file's helper.
    fn full_land_graph(geo: &Geosphere) -> ConnectionGraph {
        let mut g = ConnectionGraph::new(geo.cell_count());
        for cell in geo.cells() {
            for &n in geo.neighbors(cell) {
                if n.0 > cell.0 {
                    g.add_edge(
                        cell,
                        Edge {
                            to: n,
                            kind: EdgeKind::Adjacency,
                            conductance: 1.0,
                        },
                    );
                }
            }
        }
        g
    }

    #[test]
    fn nearest_occupied_finds_the_closest_occupied_cell_over_the_graph() {
        // full-land graph over Geosphere::new(1); occupy cells 3 and 20; from
        // cell 0, whichever is fewer graph-hops away wins (lowest CellId
        // breaks a tie). `nearest_occupied` is a free function (not a `Bake`
        // method) precisely so this test can hand-build the graph +
        // `node_index` without constructing a full `Bake`.
        let geo = Geosphere::new(1);
        let graph = full_land_graph(&geo);

        let mut node_index: BTreeMap<CellId, usize> = BTreeMap::new();
        node_index.insert(CellId(3), 0);
        node_index.insert(CellId(20), 1);

        let hops_3 = geo
            .hops_between(CellId(0), CellId(3), 16)
            .expect("cell 3 reachable");
        let hops_20 = geo
            .hops_between(CellId(0), CellId(20), 16)
            .expect("cell 20 reachable");
        assert_ne!(hops_3, hops_20, "fixture must not tie on hop distance");
        let expected_idx = if hops_3 < hops_20 { 0 } else { 1 };

        assert_eq!(
            nearest_occupied(&graph, &node_index, CellId(0)),
            Some(expected_idx),
            "expected the nearer occupied cell (hops_3={hops_3}, hops_20={hops_20})"
        );
    }

    #[test]
    fn relocate_founds_from_the_specific_predecessor_not_the_lineage_ancestor() {
        // Regression for a review finding on `relocate`: a 2nd-generation
        // relocation must attribute `founded_from` to the community that JUST
        // closed (its specific predecessor), not to the lineage's original
        // ancestor. A 1st-generation move can't distinguish the two (a
        // genesis community is its own lineage root), so this drives TWO
        // successive relocations of the same lineage and checks the second.
        use hornvale_kernel::ReferenceElevation;

        let geo = Geosphere::new(1);
        let graphs = vec![full_land_graph(&geo)];
        let capacity = CellMap::from_fn(&geo, |_| 100.0);
        let river_prox = CellMap::from_fn(&geo, |_| 0.0);
        let refugia = CellMap::from_fn(&geo, |_| false);
        let era = EraClimate {
            day: 0.0,
            ice: CellMap::from_fn(&geo, |_| false),
            habitable: CellMap::from_fn(&geo, |_| true),
            sea_level: ReferenceElevation::new(0.0).unwrap(),
            ice_fraction: 0.0,
        };
        let people = KindId("goblin");

        let mut bake = Bake {
            graphs: &graphs,
            cur_graph: 0,
            capacity: &capacity,
            river_prox: &river_prox,
            refugia: &refugia,
            records: Vec::new(),
            communities: Vec::new(),
            node_index: BTreeMap::new(),
            next_id: 1,
            stream: Seed(1).derive(hornvale_history::streams::BAKE).stream(),
            tally: BakeCensus::default(),
        };

        // Genesis: R1 opens at cell 5. A genesis community is its own
        // lineage root.
        let r1_idx = bake.open(
            people,
            CellId(5),
            0.0,
            10.0,
            Founding::Genesis(CellId(5)),
            None,
            0.0,
        );
        let r1_id = bake.communities[r1_idx].id;
        let lineage = bake.communities[r1_idx].lineage;
        assert_eq!(r1_id, lineage, "genesis community is its own lineage root");

        // First migration: R1 closes and relocates to vacant land, founded
        // from R1's own id — which equals `lineage` here, so this move alone
        // can't distinguish the bug from the fix.
        bake.close(r1_idx, 100.0, CauseOfEnd::Migrated, Ended::Nature);
        let outcome1 = bake.relocate(people, 9.0, lineage, r1_id, 0.0, CellId(5), &era, 100.0, 0);
        let r2_idx = match outcome1 {
            Relocation::Settled { cascade: 0 } => bake.communities.len() - 1,
            other => panic!("expected a direct settle onto vacant land: {other:?}"),
        };
        let r2_id = bake.communities[r2_idx].id;
        let r2_site = bake.communities[r2_idx].site;
        assert_eq!(
            bake.records[bake.communities[r2_idx].record].founded_from,
            Founding::From(r1_id)
        );

        // Second migration: R2 closes and relocates again. Its predecessor is
        // R2's OWN id — distinct from the lineage root (R1's id) — so this is
        // the case that catches the bug: the buggy code named `lineage`
        // (R1), the fix names R2.
        bake.close(r2_idx, 200.0, CauseOfEnd::Migrated, Ended::Nature);
        let outcome2 = bake.relocate(people, 8.0, lineage, r2_id, 0.0, r2_site, &era, 200.0, 0);
        let r3_idx = match outcome2 {
            Relocation::Settled { cascade: 0 } => bake.communities.len() - 1,
            other => panic!("expected a direct settle onto vacant land: {other:?}"),
        };
        let founded_from = bake.records[bake.communities[r3_idx].record].founded_from;
        assert_eq!(
            founded_from,
            Founding::From(r2_id),
            "must name the specific predecessor (R2), not the lineage ancestor"
        );
        assert_ne!(
            founded_from,
            Founding::From(lineage),
            "must NOT be the lineage ancestor (R1) for a 2nd-generation move"
        );
    }

    #[test]
    fn record_cascade_bins_by_log2_and_skips_zero() {
        // bin i covers sizes [2^i, 2^(i+1)); size 0 is not a cascade.
        let mut c = BakeCensus::default();
        c.record_cascade(0);
        assert_eq!(c.cascade_hist, [0u64; CASCADE_BINS], "size 0 not recorded");

        c.record_cascade(1); // bin 0: [1, 2)
        c.record_cascade(2); // bin 1: [2, 4)
        c.record_cascade(3); // bin 1: [2, 4)
        c.record_cascade(4); // bin 2: [4, 8)
        assert_eq!(c.cascade_hist[0], 1);
        assert_eq!(c.cascade_hist[1], 2);
        assert_eq!(c.cascade_hist[2], 1);

        // A huge cascade clamps into the top bin instead of panicking/wrapping.
        c.record_cascade(u32::MAX);
        assert_eq!(c.cascade_hist[CASCADE_BINS - 1], 1);

        let h = History::new(Vec::new(), 0.0);
        assert_eq!(
            cascade_sizes(&h),
            [0u64; CASCADE_BINS],
            "hand-built history starts at zero"
        );
    }
}
