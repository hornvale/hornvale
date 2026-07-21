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
//! displacement — communities fleeing and resettling — must GENUINELY fire,
//! at volume, driven by the era mask changing under the communities, never by
//! a floor. When the glacial era turns a community's cell hostile it migrates
//! into the shrinking habitable refugia; the refugia crowd, per-community
//! pressure crosses 1.0, and raids (hence flees and resettlements) fire as a
//! consequence of the paleoclimate swing. There is no minimum anywhere in
//! this file: if displacement comes out inert, the era swing / thresholds /
//! seeding are wrong, not the measurement.
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
use hornvale_kernel::{CellId, CellMap, EntityId, Geosphere, KindId, Seed, Stream};
use hornvale_paleoclimate::EraClimate;
use std::collections::{BTreeMap, BTreeSet};

/// Per-capita resource need. Pressure is `population * NEED / eff_capacity`;
/// kept an explicit constant so the pressure formula reads as the algorithm.
const NEED: f64 = 1.0;
/// Base per-epoch growth rate, damped logistically by `(1 - pressure)` so a
/// community asymptotes at its cell's effective capacity and — in a stable
/// era — never crosses into the raid regime on its own. Only a mask change
/// (migration into low-capacity refugia) pushes pressure past 1.0.
const GROWTH_RATE: f64 = 0.2;
/// Fraction of a community's population that survives an orderly migration to
/// a new cell (the rest is lost on the journey).
const MIGRATE_SURVIVAL: f64 = 0.9;
/// Fraction of a raided community's population the raider seizes; the
/// remainder flees with the community to a new site.
const RAID_SEIZE: f64 = 0.5;
/// Pressure at or above which a community starves out (Famine).
const COLLAPSE_PRESSURE: f64 = 2.0;
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

/// A tally of the events a bake resolved — the falsification instrument. The
/// campaign lives or dies on `fled + resettled` firing at volume.
/// type-audit: bare-ok(count: grew), bare-ok(count: founded), bare-ok(count: migrated), bare-ok(count: raided), bare-ok(count: fled), bare-ok(count: collapsed), bare-ok(count: resettled), bare-ok(count: records_total), bare-ok(count: alive_at_now)
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct BakeCensus {
    /// Grow events (a community expanded under a sub-capacity load).
    pub grew: u64,
    /// Daughter-founding events (a comfortable community spawned a daughter).
    pub founded: u64,
    /// Migration events (a community relocated off a cell turned hostile).
    pub migrated: u64,
    /// Raid events (a crowded community seized a neighbour's population).
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
}

/// Read the event census off a baked history.
pub fn census(h: &History) -> BakeCensus {
    h.tally
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
    /// The region graph.
    geo: &'a Geosphere,
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

impl<'a> Bake<'a> {
    /// Mint a fresh, never-reused entity id.
    fn mint(&mut self) -> EntityId {
        let id = EntityId::new(self.next_id).expect("entity ids start at 1");
        self.next_id += 1;
        id
    }

    /// The era in effect for `year`: the last era whose `day` is at or before
    /// `year` (piecewise-constant), or the earliest era for years before it.
    fn era_for<'e>(&self, eras: &'e [EraClimate], year: f64) -> &'e EraClimate {
        let mut chosen = &eras[0];
        for e in eras {
            if e.day <= year {
                chosen = e;
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
                for &n in self.geo.neighbors(c) {
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

    /// The wealthiest alive community adjacent to `site` (raid target),
    /// tie-broken by lowest `CellId`. `None` if no occupied neighbour.
    fn raid_target(&self, site: CellId) -> Option<usize> {
        let mut best: Option<(usize, f64, CellId)> = None;
        for &n in self.geo.neighbors(site) {
            if let Some(&idx) = self.node_index.get(&n) {
                let pop = self.communities[idx].population;
                let better = match best {
                    None => true,
                    Some((_, bp, bc)) => pop.total_cmp(&bp).then(bc.cmp(&n)).is_gt(),
                };
                if better {
                    best = Some((idx, pop, n));
                }
            }
        }
        best.map(|(idx, _, _)| idx)
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

    /// Resolve one community for one epoch (grow / found / migrate / raid /
    /// collapse). Newly opened communities are processed the following epoch.
    fn step_community(&mut self, idx: usize, era: &EraClimate, year: f64) {
        if !self.communities[idx].alive {
            return;
        }
        let site = self.communities[idx].site;
        let eff = self.eff_capacity(era, site);

        // The cell turned hostile: migrate to the nearest refuge, or die.
        if eff == 0.0 {
            match self.nearest_dest(era, site) {
                Some(dest) => {
                    let (people, pop, lineage, offset) = {
                        let c = &self.communities[idx];
                        (c.record, c.population, c.lineage, c.tech_offset)
                    };
                    let people = self.records[people].people;
                    let community_id = self.communities[idx].id;
                    self.close(idx, year, CauseOfEnd::Migrated, Ended::Nature);
                    let new_idx = self.open(
                        people,
                        dest,
                        year,
                        pop * MIGRATE_SURVIVAL,
                        Founding::From(community_id),
                        Some(lineage),
                        offset,
                    );
                    self.touch(new_idx, year);
                    self.tally.migrated += 1;
                }
                None => {
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
        } else if pressure >= 1.0 {
            self.raid(idx, era, year);
        } else {
            self.grow(idx, era, year, pressure);
        }
    }

    /// A crowded community raids its wealthiest occupied neighbour: it seizes a
    /// fraction of that community's population; the raided community flees
    /// (closing with `Fled`, `ended_by = By(raider)`) and refounds on the
    /// nearest vacant habitable cell, or is lost if none.
    fn raid(&mut self, raider: usize, era: &EraClimate, year: f64) {
        let site = self.communities[raider].site;
        let Some(target) = self.raid_target(site) else {
            return; // nothing to raid; the community simply persists this epoch.
        };
        self.tally.raided += 1;

        let seized = self.communities[target].population * RAID_SEIZE;
        self.communities[raider].population += seized;
        self.communities[target].population -= seized;
        self.touch(raider, year);

        // The raided community flees its site.
        let (people, remaining, lineage, offset, target_id) = {
            let c = &self.communities[target];
            let people = self.records[c.record].people;
            (people, c.population, c.lineage, c.tech_offset, c.id)
        };
        let raider_id = self.communities[raider].id;
        let flee_site = self.communities[target].site;
        self.close(target, year, CauseOfEnd::Fled, Ended::By(raider_id));
        self.tally.fled += 1;

        // Refound on the nearest vacant habitable cell (excluding the site it
        // just abandoned), or be lost.
        if let Some(dest) = self.nearest_dest(era, flee_site) {
            let new_idx = self.open(
                people,
                dest,
                year,
                remaining,
                Founding::From(target_id),
                Some(lineage),
                offset,
            );
            self.touch(new_idx, year);
            self.tally.resettled += 1;
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
            let dest = self
                .geo
                .neighbors(site)
                .iter()
                .copied()
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
) -> History {
    let mut bake = Bake {
        geo,
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
            .derive(people.0)
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
        let era = bake.era_for(eras, year).clone();
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
