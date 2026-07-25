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
/// How much more a HELD cell is worth than an empty cell of equal effective
/// capacity, to a people looking for a home (spec §4.1): pioneering unknown
/// ground is a gamble, a rival's holding comes already made to work. This is
/// the only term in the model that *increases* conflict — every inhibition in
/// spec §4.2a reduces it, and the ratio between them is what makes the
/// branching ratio a measurable quantity rather than a structurally-zero one.
/// A named starting value, not a fitted one. A save-format constant: changing
/// it re-fights every world's history.
const SETTLED_PREMIUM: f64 = 0.25;
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
/// [`Bake::maybe_raid`] calls for every raid whose displaced loser had to evict
/// someone in turn. A displaced people takes the best home it can get —
/// marginal vacant ground or a rich holding it can beat (see
/// [`Bake::best_home`]) — so an all-zero histogram now means the losers of this
/// world's raids were too weak to displace anybody, a measurement of the
/// branching ratio rather than an artifact of the rule.
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

/// One scored option in [`Bake::best_home`]'s single comparison: the cell, what
/// it is worth to a homeless people (the settled premium already applied when
/// it is held), the strength defending it, and its holder if it has one.
#[derive(Clone, Copy, Debug)]
struct HomeOption {
    /// The cell being scored.
    cell: CellId,
    /// Its worth to a homeless people — `eff_capacity`, times
    /// `1 + SETTLED_PREMIUM` when the cell is held.
    score: f64,
    /// The strength defending it (0.0 when the cell is vacant).
    defender: f64,
    /// The community holding it, if any — `None` is vacant land.
    holder: Option<usize>,
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

/// The raiding strength of a HOMELESS people mid-roll. It has no live
/// community to read a `tech` off, so strength is reckoned from the population
/// it still carries and the horizon its people has reached this year
/// (`tech_for(year + offset)`, the same offset the community carried) — the
/// same `population × tech_weight` reckoning [`Bake::strength`] applies to a
/// seated one. A displaced people is not disarmed by being displaced.
fn roller_strength(pop: f64, offset: f64, year: f64) -> f64 {
    pop * tech_weight(tech_for(year + offset))
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

    /// The best home a homeless people can take from `from` — spec §4.3's
    /// **one comparison over every reachable cell**, and the whole of the
    /// roll-downhill's decision. Every cell reachable over the era graph
    /// (excluding `from`, which the people has just been driven off and which
    /// its displacer now holds) is scored once:
    ///
    /// - a **vacant** habitable cell scores its effective capacity;
    /// - a **held** habitable cell scores `eff_capacity × (1 + SETTLED_PREMIUM)`
    ///   — proven ground is worth more — and is admissible only when the roller
    ///   clears `RAID_MARGIN` over its holder, and only when the roller could
    ///   still seat itself after the war it would have to fight (`can_fight`);
    /// - a cell the era's mask has made uninhabitable is worth nothing to
    ///   anybody and is not an option at all.
    ///
    /// The best score wins, tie-broken by the WEAKEST defender (vacant land
    /// defends with 0) and then the lowest `CellId` — the same total,
    /// deterministic chain [`Bake::maybe_raid`] uses, `f64::total_cmp`
    /// throughout. `None` means nothing at all is admissible.
    ///
    /// There is no `if migrating else raiding` branch here: a strong remnant
    /// preys because held ground scores higher, a weak one pioneers because
    /// held ground never enters its option set. The scan is wider than a
    /// seated raider's (which sees only its own neighbours) for the reason
    /// spec §4.3 gives — a seated people is comparing against what it already
    /// holds and is going nowhere, while a homeless one is already on the move
    /// and holds nothing.
    fn best_home(
        &self,
        era: &EraClimate,
        from: CellId,
        strength: f64,
        can_fight: bool,
    ) -> Option<HomeOption> {
        let mut best: Option<HomeOption> = None;
        let mut visited: BTreeSet<CellId> = BTreeSet::new();
        visited.insert(from);
        let mut frontier: Vec<CellId> = vec![from];
        while !frontier.is_empty() {
            let mut next: Vec<CellId> = Vec::new();
            for &c in &frontier {
                for n in traversable_neighbors(self.cur(), c) {
                    if !visited.insert(n) {
                        continue;
                    }
                    next.push(n);
                    if Self::factor(era, n) <= 0.0 {
                        continue; // the ice has made it worthless to everyone
                    }
                    let value = self.eff_capacity(era, n);
                    let (score, defender, holder) = match self.node_index.get(&n) {
                        None => (value, 0.0, None),
                        Some(&h) => {
                            let hs = self.strength(h);
                            if !can_fight || strength <= hs * RAID_MARGIN {
                                continue; // not a fight this people can win, or survive winning
                            }
                            (value * (1.0 + SETTLED_PREMIUM), hs, Some(h))
                        }
                    };
                    let better = match best {
                        None => true,
                        Some(b) => score
                            .total_cmp(&b.score) // the MOST valuable home
                            .then(b.defender.total_cmp(&defender)) // among equals, the WEAKEST held
                            .then(b.cell.cmp(&n)) // then the lowest CellId
                            .is_gt(),
                    };
                    if better {
                        best = Some(HomeOption {
                            cell: n,
                            score,
                            defender,
                            holder,
                        });
                    }
                }
            }
            frontier = next;
        }
        best
    }

    /// Relocate a homeless people (a remnant driven off its land by a raid) to
    /// a new home, cascading when the home it wants is already held.
    /// `predecessor` is the id of the community that just closed and is
    /// relocating (used to attribute the new occupation's `Founding::From` to
    /// its specific forebear, not the lineage ancestor — `lineage` stays
    /// reserved for the `open` lineage argument). Returns the outcome:
    /// [`Relocation::Settled`] (with the cascade size — the number of OCCUPIED
    /// cells this relocation displaced, 0 if it took vacant land) or
    /// [`Relocation::Lost`] (the remnant died, nothing was admissible, or the
    /// depth cap truncated the chain).
    ///
    /// The roll-downhill is spec §4.3's "re-enters the raid rule", taken
    /// literally: [`Bake::best_home`] makes ONE comparison over every reachable
    /// cell, and if the winner is held, its occupant is evicted and relocates
    /// in turn. War is lossy on both sides of that eviction exactly as it is in
    /// [`Bake::maybe_raid`], so a chain dissipates fast: each hop costs the
    /// roller `WAR_LOSS` and the victim `WAR_LOSS` plus the journey, and each
    /// victim is by construction at least `RAID_MARGIN` times weaker than the
    /// people that displaced it. A cascade therefore terminates because it runs
    /// out of strength (`VIABLE_MIN`), not because `CASCADE_DEPTH_CAP` catches
    /// it — the cap is the safety bound, not the physics.
    ///
    /// Every remnant that ends `Lost` inside the chain is tallied as a
    /// `collapsed` community at the call site that lost it, exactly as the
    /// top-level caller tallies its own: a community may not vanish from the
    /// world uncounted.
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
        // A people that could not seat itself after paying the war loss cannot
        // take held land at all (it would `open` at `peak_population == 0` — a
        // peopleless settlement, which the shipped invariant forbids). It can
        // still pioneer.
        let can_fight = pop * (1.0 - WAR_LOSS) >= VIABLE_MIN;
        let Some(home) = self.best_home(era, from, roller_strength(pop, offset, year), can_fight)
        else {
            return Relocation::Lost; // nothing vacant, nothing beatable — lost
        };
        let Some(victim) = home.holder else {
            // Vacant land won the comparison — no conflict, settle there.
            let new_idx = self.open(
                people,
                home.cell,
                year,
                pop,
                Founding::From(predecessor),
                Some(lineage),
                offset,
            );
            self.touch(new_idx, year);
            return Relocation::Settled { cascade: 0 };
        };

        // Held land won: the roller takes it by force. War is lossy on BOTH
        // sides here, as in `maybe_raid` — spec §4.3 destroys a fraction of the
        // COMBINED population, and it is that dissipation (compounding down the
        // chain) that terminates an avalanche.
        self.communities[victim].population *= 1.0 - WAR_LOSS;
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
        // node_index[home.cell] points at the new occupant; close then sees
        // the cell already re-indexed and does not free it).
        let new_idx = self.open(
            people,
            home.cell,
            year,
            pop * (1.0 - WAR_LOSS),
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
        // victim's) community id, carrying what the war and the road left it.
        let victim_cascade = match self.relocate(
            v_people,
            v_pop * MIGRATE_SURVIVAL,
            v_lineage,
            v_id,
            v_offset,
            home.cell,
            era,
            year,
            depth + 1,
        ) {
            Relocation::Settled { cascade } => cascade,
            Relocation::Lost => {
                // It died on the road. Count it: the top-level caller maps its
                // own `Lost` to a collapse, and a community lost deeper in the
                // chain is no less gone from the world.
                self.tally.collapsed += 1;
                0
            }
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

    /// The owned inputs a hand-built [`Bake`] borrows, over `Geosphere::new(1)`
    /// with a full-land graph and every cell habitable in the single era.
    /// `capacity` is `POOR` everywhere except the cells listed in `rich`, which
    /// get `RICH` — the value gradient the roll-downhill tests need.
    fn cascade_world(
        rich: &[CellId],
    ) -> (
        Geosphere,
        Vec<ConnectionGraph>,
        CellMap<f64>,
        CellMap<f64>,
        CellMap<bool>,
        EraClimate,
    ) {
        use hornvale_kernel::ReferenceElevation;
        let geo = Geosphere::new(1);
        let graphs = vec![full_land_graph(&geo)];
        let capacity = CellMap::from_fn(&geo, |c| if rich.contains(&c) { RICH } else { POOR });
        let river_prox = CellMap::from_fn(&geo, |_| 0.0);
        let refugia = CellMap::from_fn(&geo, |_| false);
        let era = EraClimate {
            day: 0.0,
            ice: CellMap::from_fn(&geo, |_| false),
            habitable: CellMap::from_fn(&geo, |_| true),
            sea_level: ReferenceElevation::new(0.0).unwrap(),
            ice_fraction: 0.0,
        };
        (geo, graphs, capacity, river_prox, refugia, era)
    }

    /// Marginal land in [`cascade_world`] — a cell worth taking only when
    /// nothing better is admissible.
    const POOR: f64 = 10.0;
    /// Prime land in [`cascade_world`] — ten times a poor cell's worth.
    const RICH: f64 = 100.0;

    /// A hand-built [`Bake`] over [`cascade_world`]'s inputs, with an empty
    /// record set and a fixed stream.
    fn hand_bake<'a>(
        graphs: &'a [ConnectionGraph],
        capacity: &'a CellMap<f64>,
        river_prox: &'a CellMap<f64>,
        refugia: &'a CellMap<bool>,
    ) -> Bake<'a> {
        Bake {
            graphs,
            cur_graph: 0,
            capacity,
            river_prox,
            refugia,
            records: Vec::new(),
            communities: Vec::new(),
            node_index: BTreeMap::new(),
            next_id: 1,
            stream: Seed(1).derive(hornvale_history::streams::BAKE).stream(),
            tally: BakeCensus::default(),
        }
    }

    #[test]
    fn a_roller_takes_the_rich_held_cell_over_the_marginal_vacant_one() {
        // Spec §4.3's amended rule, and the whole reason Task 1 measured a
        // structurally-zero branching ratio: a displaced people compares every
        // reachable cell in ONE pass. A rich cell held by a beatable neighbour
        // outbids marginal vacant land, so the roller preys rather than
        // pioneering — and the holder it evicts rolls onward. Under the
        // vacant-first rule this returns `cascade: 0` and no cascade is ever
        // recorded.
        let (_geo, graphs, capacity, river_prox, refugia, era) = cascade_world(&[CellId(20)]);
        let mut bake = hand_bake(&graphs, &capacity, &river_prox, &refugia);

        // A weak community sits on the one rich cell; a strong people is
        // driven off cell 0 (poor land) and must find a home.
        let holder = bake.open(
            KindId("goblin"),
            CellId(20),
            0.0,
            5.0,
            Founding::Genesis(CellId(20)),
            None,
            0.0,
        );
        let roller = bake.open(
            KindId("kobold"),
            CellId(0),
            0.0,
            50.0,
            Founding::Genesis(CellId(0)),
            None,
            0.0,
        );
        let (r_id, r_lineage) = (
            bake.communities[roller].id,
            bake.communities[roller].lineage,
        );
        bake.close(roller, 0.0, CauseOfEnd::Fled, Ended::Nature);

        let outcome = bake.relocate(
            KindId("kobold"),
            50.0,
            r_lineage,
            r_id,
            0.0,
            CellId(0),
            &era,
            0.0,
            0,
        );
        assert_eq!(
            outcome,
            Relocation::Settled { cascade: 1 },
            "the roller must displace the rich cell's holder, not settle marginal vacant land"
        );

        // The rich cell is now held by the roller's people, at its post-war
        // population, and the holder is dead and driven off.
        let seated = *bake
            .node_index
            .get(&CellId(20))
            .expect("the rich cell must be occupied");
        assert_eq!(
            bake.records[bake.communities[seated].record].people,
            KindId("kobold"),
            "the roller must be the one seated on the rich cell"
        );
        assert!(
            (bake.communities[seated].population - 50.0 * (1.0 - WAR_LOSS)).abs() < 1e-9,
            "the roller must pay the war loss to take held land: {}",
            bake.communities[seated].population
        );
        assert!(
            !bake.communities[holder].alive,
            "the holder must be evicted"
        );
        assert_eq!(
            bake.records[bake.communities[holder].record].cause,
            Some(CauseOfEnd::Fled)
        );
        // The evicted holder rolled onward and found marginal vacant land.
        assert!(
            bake.communities.iter().any(|c| c.alive
                && c.site != CellId(20)
                && c.lineage == bake.communities[holder].lineage),
            "the evicted holder must have resettled somewhere"
        );
        assert_eq!((bake.tally.raided, bake.tally.fled), (1, 1));
    }

    #[test]
    fn the_settled_premium_makes_a_held_cell_outbid_an_equal_vacant_one() {
        // The only term in the model that RAISES conflict (spec §4.1): a held
        // cell is worth more than an empty cell of equal capacity, because a
        // rival's holding comes already made to work. With the premium at 0
        // the roller takes the equally-rich EMPTY cell (no defender) and the
        // branching ratio collapses again.
        let (_geo, graphs, capacity, river_prox, refugia, era) =
            cascade_world(&[CellId(20), CellId(30)]);
        let mut bake = hand_bake(&graphs, &capacity, &river_prox, &refugia);

        // Cell 20 is rich AND held by a beatable community; cell 30 is rich
        // and empty. Equal capacity — only the premium separates them.
        bake.open(
            KindId("goblin"),
            CellId(20),
            0.0,
            5.0,
            Founding::Genesis(CellId(20)),
            None,
            0.0,
        );
        let roller = bake.open(
            KindId("kobold"),
            CellId(0),
            0.0,
            50.0,
            Founding::Genesis(CellId(0)),
            None,
            0.0,
        );
        let (r_id, r_lineage) = (
            bake.communities[roller].id,
            bake.communities[roller].lineage,
        );
        bake.close(roller, 0.0, CauseOfEnd::Fled, Ended::Nature);

        let outcome = bake.relocate(
            KindId("kobold"),
            50.0,
            r_lineage,
            r_id,
            0.0,
            CellId(0),
            &era,
            0.0,
            0,
        );
        assert_eq!(
            outcome,
            Relocation::Settled { cascade: 1 },
            "the premium must make the HELD rich cell outbid the equally-rich empty one"
        );
        let seated = *bake
            .node_index
            .get(&CellId(20))
            .expect("the held rich cell must have changed hands");
        assert_eq!(
            bake.records[bake.communities[seated].record].people,
            KindId("kobold")
        );
    }

    #[test]
    fn a_weak_roller_flees_to_the_empties_instead_of_preying() {
        // The emergent half of spec §4.3: there is no `if migrating else
        // raiding` branch. A remnant too weak to clear the dominance margin
        // never sees the held cell in its option set at all, so it pioneers —
        // the same one rule, a different outcome.
        let (_geo, graphs, capacity, river_prox, refugia, era) = cascade_world(&[CellId(20)]);
        let mut bake = hand_bake(&graphs, &capacity, &river_prox, &refugia);

        let holder = bake.open(
            KindId("goblin"),
            CellId(20),
            0.0,
            5.0,
            Founding::Genesis(CellId(20)),
            None,
            0.0,
        );
        let holder_id = bake.communities[holder].id;
        let roller = bake.open(
            KindId("kobold"),
            CellId(0),
            0.0,
            3.0,
            Founding::Genesis(CellId(0)),
            None,
            0.0,
        );
        let (r_id, r_lineage) = (
            bake.communities[roller].id,
            bake.communities[roller].lineage,
        );
        bake.close(roller, 0.0, CauseOfEnd::Fled, Ended::Nature);

        // Strength 3.0 does not clear 5.0 × RAID_MARGIN, so cell 20 is not an
        // option however rich it is.
        let outcome = bake.relocate(
            KindId("kobold"),
            3.0,
            r_lineage,
            r_id,
            0.0,
            CellId(0),
            &era,
            0.0,
            0,
        );
        assert_eq!(
            outcome,
            Relocation::Settled { cascade: 0 },
            "a roller that beats nobody must pioneer, not prey"
        );
        assert_eq!(
            bake.communities[*bake.node_index.get(&CellId(20)).expect("still held")].id,
            holder_id,
            "the holder must be untouched"
        );
        assert_eq!((bake.tally.raided, bake.tally.fled), (0, 0));
    }

    #[test]
    fn the_depth_cap_truncates_a_cascade_and_the_dropped_remnant_is_tallied() {
        // Two bounds in one fixture. (a) `CASCADE_DEPTH_CAP` is a hard stop:
        // at the cap nothing is opened at all. (b) One hop below it, the
        // displacement still happens and the victim's own relocation is
        // truncated — and that lost victim MUST be counted (a Task-1 review
        // defect: the recursion dropped it silently while the top-level call
        // mapped `Lost` to `collapsed`, so communities vanished uncounted).
        let (_geo, graphs, capacity, river_prox, refugia, era) = cascade_world(&[CellId(20)]);
        let mut bake = hand_bake(&graphs, &capacity, &river_prox, &refugia);

        let holder = bake.open(
            KindId("goblin"),
            CellId(20),
            0.0,
            5.0,
            Founding::Genesis(CellId(20)),
            None,
            0.0,
        );
        let holder_lineage = bake.communities[holder].lineage;
        let roller = bake.open(
            KindId("kobold"),
            CellId(0),
            0.0,
            50.0,
            Founding::Genesis(CellId(0)),
            None,
            0.0,
        );
        let (r_id, r_lineage) = (
            bake.communities[roller].id,
            bake.communities[roller].lineage,
        );
        bake.close(roller, 0.0, CauseOfEnd::Fled, Ended::Nature);
        let records_before = bake.records.len();

        // (a) AT the cap: nothing may happen at all.
        let capped = bake.relocate(
            KindId("kobold"),
            50.0,
            r_lineage,
            r_id,
            0.0,
            CellId(0),
            &era,
            0.0,
            CASCADE_DEPTH_CAP,
        );
        assert_eq!(
            capped,
            Relocation::Lost,
            "the depth cap must stop the chain"
        );
        assert_eq!(
            bake.records.len(),
            records_before,
            "a capped relocation must open nothing"
        );

        // (b) ONE HOP below the cap: the roller displaces, and the victim's
        //     own relocation hits the cap and is lost — and tallied.
        let outcome = bake.relocate(
            KindId("kobold"),
            50.0,
            r_lineage,
            r_id,
            0.0,
            CellId(0),
            &era,
            0.0,
            CASCADE_DEPTH_CAP - 1,
        );
        assert_eq!(
            outcome,
            Relocation::Settled { cascade: 1 },
            "the last admissible hop must still displace"
        );
        assert_eq!(
            bake.tally.collapsed, 1,
            "the truncated victim must be counted as a collapse, not dropped silently"
        );
        assert!(
            !bake
                .communities
                .iter()
                .any(|c| c.alive && c.lineage == holder_lineage),
            "the truncated victim must be gone from the world"
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
