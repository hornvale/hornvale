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
/// Pressure at or above which a community has NO SPOILS: it already eats
/// everything its own land yields, so there is no surplus for a conqueror to
/// take and nothing to contend over (spec §4.2a's momentary inhibition). One
/// is the natural threshold and not a fitted one — it is exactly where the
/// logistic growth term `1 + GROWTH_RATE × (1 - pressure)` stops being
/// positive, i.e. where the file's own model says the land no longer feeds
/// the people on it. Well below `COLLAPSE_PRESSURE`: a community can be
/// worthless to a raider long before it starves out altogether.
const NO_SPOILS_PRESSURE: f64 = 1.0;
/// `threat_response` (flee 0 ↔ stand 1) at or above which a people takes the
/// initiative at all — spec §4.2a's durable inhibition. Authored species data,
/// never drawn.
///
/// **Disclosure on the value.** This gate's stated purpose is to make raiding
/// heterogeneous *across peoples*; a threshold that admits the whole roster, or
/// vetoes the whole roster, is inert by construction and buys nothing. 0.6 is
/// chosen to sit between the registry's declared baseline temperament (goblin,
/// 0.5) and the assertive peoples (hobgoblin 0.7, kobold and bugbear 0.8), so
/// exactly one of the four settling peoples declines to raid. That is a choice
/// about what the gate *means*, made against the authored roster — not one
/// fitted to a measured outcome; the cascade metric was not consulted in
/// picking it. A save-format constant: changing it re-fights every world's
/// history.
const RAID_DISPOSITION_MIN: f64 = 0.6;
/// Pressure at or above which a community starves out (Famine). `pub` so the
/// demography calibration (`windows/lab/tests/gathering_calibration.rs`) can
/// express its world-scale population ceiling, which since The Tumult reads
/// `Σ peak_pop ≤ COLLAPSE_PRESSURE × SETTLERS_PER_CAPACITY ×
/// Σ suitability(OCCUPIED cells)` — a sum of committed per-record *peaks*
/// over exactly the cells the live settlements sit on, not instantaneous
/// population over the whole world. That gate's own doc comment is the
/// authority on what the inequality does and does not establish (it is a
/// runaway detector, not a per-community bound); do not restate it here.
/// type-audit: bare-ok(ratio)
pub const COLLAPSE_PRESSURE: f64 = 2.0;
/// Pressure below which a comfortable community may throw off a daughter.
const DAUGHTER_MAX_PRESSURE: f64 = 0.7;
/// Per-epoch probability a comfortable community founds a daughter.
const DAUGHTER_PROB: f64 = 0.06;
/// How much a unit of stored wealth is worth as raiding strength, relative to
/// a head of population. Walls, retainers and granaries are strength the local
/// land does not have to feed.
const STORE_WEIGHT: f64 = 0.5;
/// The fraction of a community's stores that survives each epoch — a hoard is
/// not immortal.
const STORE_DECAY: f64 = 0.95;
/// The share of a subordinate cell's effective capacity a patron demands per
/// epoch. The dominant taxes what it can SEE — the land, never the granary
/// (spec §4.2's information asymmetry). A save-format constant: changing it
/// re-fights every world's history.
const ASSESS_RATE: f64 = 0.1;
/// The ceiling on an assessment, as a multiple of the subordinate cell's
/// effective capacity: no patron may demand more than the land could ever
/// produce (spec §4.5's divergence bound). It does not bind at the moment a
/// relation forms — `ASSESS_RATE` is well under it — but it is the bound the
/// deferred adaptive-demand loop (§4.3) raises an assessment against, so the
/// clamp is written where the assessment is set rather than bolted on later.
const ASSESS_MAX: f64 = 0.5;
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

/// Configuration for a deep-history bake: the span of years to simulate, the
/// epoch step, and the authored per-people disposition the raid rule's durable
/// inhibition reads. Years are bare `f64` (absolute, no wall-clock). Not
/// `Copy`: `disposition` is an owned map, and the config is always passed by
/// reference.
/// type-audit: bare-ok(count: start_year), bare-ok(count: end_year), bare-ok(count: epoch_years), bare-ok(ratio: disposition)
#[derive(Clone, Debug, PartialEq)]
pub struct BakeConfig {
    /// The year the ancient world is seeded at (inclusive).
    pub start_year: f64,
    /// The year the bake closes at (`now`); alive records keep `ended = None`.
    pub end_year: f64,
    /// The step between epochs, in years.
    pub epoch_years: f64,
    /// Each people's `threat_response` (flee 0 ↔ stand 1) — authored species
    /// data, never drawn, looked up by the composition root and handed in here
    /// because the bake reads only kernel types. A people below
    /// `RAID_DISPOSITION_MIN` never takes the initiative (spec §4.2a's durable
    /// inhibition); a people ABSENT from the map is not vetoed, so a bake given
    /// no disposition data behaves exactly as it did before the gate existed.
    pub disposition: BTreeMap<KindId, f64>,
}

impl BakeConfig {
    /// The default bake span: two millennia in 25-year epochs, with no
    /// authored disposition (nobody vetoed — the composition root fills it in).
    pub fn default_millennia() -> BakeConfig {
        BakeConfig {
            start_year: 0.0,
            end_year: 2000.0,
            epoch_years: 25.0,
            disposition: BTreeMap::new(),
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
    /// The tribute relations still standing at `now`, in subordinate order.
    /// Only the survivors: a relation ends when either party's community
    /// closes (spec §4.4), so one that formed and dissolved mid-span leaves
    /// nothing here — exactly as a dead occupation leaves a ruin rather than a
    /// settlement.
    pub tribute: Vec<TributeRelation>,
    /// Event tallies, counted as the bake resolves each epoch.
    tally: BakeCensus,
}

/// A standing tribute relation as it stood at `now`, carried out of the bake
/// for emission. Both parties are named by their **community handle** (the
/// `community` field of an [`OccupationRecord`]), not by a bake-internal index,
/// so this survives the bake it came from — the same translation `Ended::By`
/// and `Founding::From` already rely on.
/// type-audit: bare-ok(count: since)
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct TributeRelation {
    /// The community that pays.
    pub subordinate: EntityId,
    /// The community that collects.
    pub patron: EntityId,
    /// The standard day this relation was established. A patronage transfer
    /// re-establishes it, so this is when the *current* patron took it over,
    /// not when the subordinate first began paying somebody.
    pub since: f64,
}

/// A tally of the events a bake resolved — the falsification instrument. Under
/// The Tumult's predation model the load-bearing counts are `raided`/`fled`
/// (conflict must fire on a *value* gradient, in worlds with land to spare, and
/// stay at zero in value-flat ones) read against `alive_at_now` (conquest must
/// redistribute the world, not depopulate it).
/// type-audit: bare-ok(count: grew), bare-ok(count: founded), bare-ok(count: migrated), bare-ok(count: raided), bare-ok(count: fled), bare-ok(count: collapsed), bare-ok(count: resettled), bare-ok(count: subordinations_formed), bare-ok(count: patronage_transfers), bare-ok(count: tribute_relations_at_now), bare-ok(count: max_subordinates), bare-ok(count: tribute_collected), bare-ok(count: max_stores_at_now), bare-ok(count: records_total), bare-ok(count: alive_at_now), bare-ok(count: cascade_hist)
// `Eq` is deliberately absent: the two accumulator readouts below are `f64`,
// and a census is only ever compared for equality in assertions (`PartialEq`),
// never used as a key.
#[derive(Clone, Copy, Debug, Default, PartialEq)]
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
    /// Resettle events (a displaced community refounded on a vacant habitable
    /// cell). Counted at every depth of a relaxation, not only at its head: a
    /// cascade's terminal roller reaches vacant ground exactly as a first-hop
    /// one does, and is a resettle by the same reading.
    pub resettled: u64,
    /// **First-time** subordinations: a dominant community imposed a standing
    /// tribute relation on a productive neighbour that was paying nobody.
    /// Deliberately excludes takeovers — a single counter mixing the two
    /// cannot be read as "N subjugations" the way spec §8.1's "at volume"
    /// criterion assumes, because churn between rival patrons inflates it
    /// without a single new people being subjugated.
    pub subordinations_formed: u64,
    /// **Takeovers**: a relation that already existed changed hands, the new
    /// patron having cleared `RAID_MARGIN` over the incumbent (spec §4.4's
    /// hysteresis). Reported beside the formations rather than pooled with
    /// them precisely so churn is visible as churn.
    pub patronage_transfers: u64,
    /// Tribute relations still standing **at `now`** — the stock, where the
    /// two counters above are flows. A relation ends when either party's
    /// community closes, so this is what survived the whole span, not what was
    /// ever formed.
    pub tribute_relations_at_now: u64,
    /// The largest number of subordinates any one patron holds **at `now`** —
    /// spec §8.2's runaway-hub reading. A standing measurement of the relation
    /// graph as the bake closes, not a peak over the whole span: a star that
    /// formed and dissolved before `now` is not in it.
    pub max_subordinates: u64,
    /// Every remittance that actually changed hands, summed over the **whole
    /// bake** — a run total (the integral of the per-epoch flows), not a
    /// per-epoch flow and not a stock. It counts what moved, not what was
    /// demanded: an assessment the subordinate's growth could not cover is
    /// collected only up to that growth, and the shortfall is not in here.
    pub tribute_collected: f64,
    /// The largest store any one **alive** community holds at `now` — spec
    /// §8.2's accumulator readout, and a stock, where `tribute_collected` above
    /// is a flow. Stores decay each epoch and are lost when a community closes,
    /// so this is what a surviving extractor has actually managed to keep, not
    /// the sum of everything it ever took.
    pub max_stores_at_now: f64,
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
/// someone in turn. A displaced people takes the best home in the nearest ring
/// that offers it one — marginal vacant ground or a rich holding it can beat
/// (see [`Bake::best_home`]) — so an all-zero histogram now means the losers of this
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
    /// `tribute` likewise starts empty — a hand-built history has no relation
    /// table behind it; a test that wants one assigns the field directly.
    /// type-audit: bare-ok(count: now)
    pub fn new(records: Vec<OccupationRecord>, now: f64) -> History {
        History {
            records,
            now,
            tribute: Vec::new(),
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
    /// Accumulated wealth — tribute, stores, the granary. Feeds raiding
    /// strength but is NEVER eaten: it does not enter the pressure term, so a
    /// successful extractor does not starve itself on its own tribute (spec
    /// §4.2a). Lost with the community when it closes.
    stores: f64,
}

/// A standing tribute relation: who a community pays, and how much its patron
/// currently demands. Live bake state only — a relation is never serialized,
/// so this adds no committed field and no save-format surface (spec §4.4).
#[derive(Clone, Copy, Debug)]
struct Tribute {
    /// Index into `Bake::communities` of the patron.
    patron: usize,
    /// What the patron currently demands per epoch — set from what it can SEE
    /// (the subordinate's cell), never from what the subordinate has
    /// (spec §4.2). Clamped to `[0, eff_capacity × ASSESS_MAX]`. Read by
    /// [`Bake::collect_tribute`], which pays out the lesser of this and the
    /// subordinate's growth: the demand is what the patron *asks*, never what
    /// it necessarily gets.
    ///
    /// The term is set the moment a relation forms because that is when the
    /// patron takes its reading of the land, not when it first comes to
    /// collect.
    assessment: f64,
    /// The standard day this relation was established — the day the *current*
    /// patron took it, since a patronage transfer re-establishes it. Carried
    /// only so the emitted fact can be dated by when it became true, exactly as
    /// an occupation's end-of-life facts are.
    since: f64,
}

/// Which way a raid resolves, decided by the **mobility of the prize**
/// (spec §4.1). Both outcomes clear the same dominance and no-spoils gates;
/// only what is takeable differs.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum Spoil {
    /// The cell is worth more: an immobile prize, takeable only by occupying
    /// it. The raid evicts and seizes (the shipped path).
    Evict,
    /// The cell is no better, but the people are productive: a mobile prize,
    /// takeable repeatedly without displacing anyone. The raid subordinates.
    Subordinate,
}

impl Spoil {
    /// Rank for the candidate comparator: **eviction outranks subordination**
    /// at equal value, because taking the land is the larger prize.
    ///
    /// This term cannot actually decide a comparison under the shipped
    /// classification — `Evict` requires `t_val > raider_val` and
    /// `Subordinate` requires `t_val <= raider_val`, so every eviction
    /// candidate already outranks every subordination candidate on value
    /// alone. It is stated because the ordering is a *decision*, and a future
    /// classification that admits an equal-value eviction must not have that
    /// decision fall to whichever cell the neighbour walk happened to visit
    /// first.
    fn rank(self) -> u8 {
        match self {
            Spoil::Evict => 1,
            Spoil::Subordinate => 0,
        }
    }
}

/// The mutable bake state: records, live communities, the one-alive-per-site
/// index, the id counter, the dynamics stream, the standing tribute relations,
/// and the running tally.
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
    /// Each people's authored `threat_response`, borrowed off the
    /// [`BakeConfig`] — the durable inhibition [`Bake::takes_the_initiative`]
    /// reads.
    disposition: &'a BTreeMap<KindId, f64>,
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
    /// Standing tribute relations, keyed by the **subordinate's** community
    /// index. Keying by subordinate makes *at most one patron per community*
    /// structural: a second bid overwrites rather than adding (spec §4.4's
    /// patronage transfer, which additionally has to clear hysteresis).
    ///
    /// **That alone does NOT give one-level stars.** Bounding out-degree to
    /// one yields a *functional graph*, which still admits chains and cycles;
    /// measurement without the further checks found 57–89% of standing
    /// relations sitting under a patron who was themselves paying someone.
    /// The one-level-star shape spec §4.4 requires is enforced explicitly, in
    /// [`Bake::maybe_raid`]'s classification: a raider that is itself a
    /// subordinate takes no vassal, and a target that is itself a patron is
    /// not subordinated. Those two keep the patron set and the subordinate set
    /// **disjoint** inductively, which is what makes depth — and therefore a
    /// cycle — impossible rather than merely unobserved. Depth is the deferred
    /// chaining lever (spec §9), and spec §5 preregisters the headline on its
    /// absence.
    ///
    /// Iterated in key order (`BTreeMap`, never a hash map).
    tribute: BTreeMap<usize, Tribute>,
    /// **This epoch's** growth increment per community, indexed exactly as
    /// `communities` is (`open` pushes a zero as it appends). A remittance is
    /// paid out of the epoch's growth and never out of the standing stock
    /// (spec §4.2), so collection needs the increment `grow` actually applied
    /// — the difference between milking a subordinate and killing it.
    ///
    /// Zeroed wholesale at the top of every epoch by [`Bake::begin_epoch`]. A
    /// stale increment surviving into the next epoch would be taxed again, and
    /// again, on a community that never grew after it — the standing stock by
    /// another name, arrived at by accident.
    ///
    /// **That clear is load-bearing**, not decorative: `grow` *accumulates*
    /// into this buffer rather than overwriting it, so an epoch that did not
    /// start from zero would carry the previous epoch's surplus forward and let
    /// it be taxed a second time (`last_epochs_growth_is_never_taxed_twice`
    /// reddens if the clear is suppressed). The invariant §4.2 rests on is
    /// "this epoch's growth", not "`grow` happens to run exactly once for
    /// everybody" — a rule that grew a community twice in an epoch, or grew
    /// only some of them, must not make the difference silent.
    epoch_growth: Vec<f64>,
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
/// (`tech_for(year + offset)`, the same offset the community carried). A
/// displaced people is not disarmed by being displaced.
///
/// This deliberately carries **no stores term**, unlike [`Bake::strength`]: a
/// roller's community has already closed, and stores are lost on closure
/// (spec §4.2a), so crediting a homeless remnant with a hoard would resurrect
/// wealth the fall of its community destroyed.
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

    /// Walk the era graph outward from `from` in breadth-first **rings** and
    /// stop at the first ring `pick` finds something in. Ring `d` is exactly
    /// the set of cells at graph distance `d` from `from`; `from` itself is
    /// never offered. Traversal passes *through* cells `pick` rejects, so a
    /// people whose whole neighbourhood is unusable still reaches the ring
    /// beyond it — this is a widening search, not a one-ring scan.
    ///
    /// **Determinism.** Ring membership is a graph property (a full ring is
    /// consumed before the next is expanded), and each ring is handed to
    /// `pick` as an ascending, deduplicated `CellId` slice, so neither the
    /// discovery order within a ring nor the order edges were added can reach
    /// the result. `pick` is still responsible for a total order among the
    /// cells of the one ring it accepts.
    ///
    /// This is the file's nearest-first idiom, shared by the two searches that
    /// need it — [`Bake::nearest_dest`] (a migrant's refuge) and
    /// [`Bake::best_home`] (a displaced people's roll-downhill).
    fn nearest_ring<T>(
        &self,
        from: CellId,
        mut pick: impl FnMut(&[CellId]) -> Option<T>,
    ) -> Option<T> {
        let mut visited: BTreeSet<CellId> = BTreeSet::new();
        visited.insert(from);
        let mut frontier: Vec<CellId> = vec![from];
        while !frontier.is_empty() {
            // A BTreeSet, so the ring reaches `pick` in ascending CellId order
            // however its cells were discovered.
            let mut ring: BTreeSet<CellId> = BTreeSet::new();
            for &c in &frontier {
                for n in traversable_neighbors(self.cur(), c) {
                    if visited.insert(n) {
                        ring.insert(n);
                    }
                }
            }
            let next: Vec<CellId> = ring.into_iter().collect();
            if let Some(found) = pick(&next) {
                return Some(found);
            }
            frontier = next;
        }
        None
    }

    /// The nearest vacant habitable cell to `from` (excluding `from` itself),
    /// by breadth-first hop distance. Within the nearest layer, refugial cells
    /// win over non-refugial, then lowest `CellId` — a total, deterministic
    /// order. `None` if the whole reachable graph is full or hostile.
    fn nearest_dest(&self, era: &EraClimate, from: CellId) -> Option<CellId> {
        self.nearest_ring(from, |ring| {
            let mut candidates: Vec<CellId> = ring
                .iter()
                .copied()
                .filter(|&n| self.vacant_habitable(era, n))
                .collect();
            if candidates.is_empty() {
                return None;
            }
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
            Some(candidates[0])
        })
    }

    /// The best home a homeless people can take from `from` — spec §4.3's
    /// **one comparison**, and the whole of the roll-downhill's decision. The
    /// scan walks the era graph outward from `from` (which the people has just
    /// been driven off and which its displacer now holds) and **stops at the
    /// first ring that contains an admissible option**, taking the best value
    /// within that ring. Within a ring:
    ///
    /// - a **vacant** habitable cell scores its effective capacity;
    /// - a **held** habitable cell scores `eff_capacity × (1 + SETTLED_PREMIUM)`
    ///   — proven ground is worth more — and is admissible only when the roller
    ///   clears `RAID_MARGIN` over its holder, only when the roller could still
    ///   seat itself after the war it would have to fight (`can_fight`), and
    ///   only when spec §4.2a's inhibitions ([`Bake::takes_the_initiative`],
    ///   [`Bake::has_spoils`]) do not veto it;
    /// - a cell the era's mask has made uninhabitable is worth nothing to
    ///   anybody and is not an option at all.
    ///
    /// The best score in the accepted ring wins, tie-broken by the WEAKEST
    /// defender (vacant land defends with 0) and then the lowest `CellId` —
    /// the same total, deterministic chain [`Bake::maybe_raid`] uses,
    /// `f64::total_cmp` throughout. `None` means nothing is admissible
    /// anywhere reachable.
    ///
    /// **Locality is part of the rule** (spec §4.3): "re-enters the raid rule"
    /// inherits the raid rule's *neighbourhood* as well as its comparison, so
    /// the settled premium decides between a vacant and a held cell **at the
    /// same distance** — the only place it should decide — and a remnant never
    /// crosses a continent for a marginally better cell. It is nonetheless a
    /// *widening* search ([`Bake::nearest_ring`]): a people whose whole
    /// neighbourhood is full or hostile still migrates as far as it must.
    ///
    /// There is no `if migrating else raiding` branch here: a strong remnant
    /// preys because held ground scores higher, a weak one pioneers because
    /// held ground never enters its option set.
    fn best_home(
        &self,
        people: KindId,
        era: &EraClimate,
        from: CellId,
        strength: f64,
        can_fight: bool,
    ) -> Option<HomeOption> {
        // Whether held ground is in this people's option set AT ALL — a
        // strictly narrower thing than the caller's `can_fight` ("would
        // survive winning"), so it gets its own name rather than shadowing the
        // parameter. The durable inhibition is a property of the people, not
        // of any one candidate: a timid people simply never sees held ground.
        let may_take_held_land = can_fight && self.takes_the_initiative(people);
        self.nearest_ring(from, |ring| {
            let mut best: Option<HomeOption> = None;
            for &n in ring {
                if Self::factor(era, n) <= 0.0 {
                    continue; // the ice has made it worthless to everyone
                }
                let value = self.eff_capacity(era, n);
                let (score, defender, holder) = match self.node_index.get(&n) {
                    None => (value, 0.0, None),
                    Some(&h) => {
                        let hs = self.strength(h);
                        if !may_take_held_land || strength <= hs * RAID_MARGIN {
                            continue; // not a fight this people can win, or survive winning
                        }
                        if !self.has_spoils(era, h) {
                            continue; // a husk: nothing to take (spec §4.2a)
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
            best
        })
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
    /// literally: [`Bake::best_home`] makes ONE comparison, over the nearest
    /// ring that holds anything admissible at all, and if the winner is held,
    /// its occupant is evicted and relocates in turn. War is lossy on both
    /// sides of that eviction exactly as it is in
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
        // A people that could not seat itself after paying the war loss does
        // not start the fight: winning would leave it below the viability floor
        // it only just cleared above, holding a prize as a remnant this model
        // already calls dead. It can still pioneer — the veto is on held ground
        // alone.
        //
        // NOT the no-peopleless-settlements guard, whatever this comment used
        // to say (The Tumult, final review F-3). That invariant is enforced
        // upstream by the `pop < VIABLE_MIN` return directly above: the
        // smallest population that reaches this line is `VIABLE_MIN`, and
        // `VIABLE_MIN × (1 - WAR_LOSS)` = 1.4 rounds to 1 in `open`, never to
        // 0. `Bake::maybe_raid`'s identically-shaped guard is the one that
        // genuinely does prevent a `peak_population == 0` seat — nothing bounds
        // a seated raider's population from below, so its post-war figure can
        // round to zero. Here the rule is viability, not headcount, and
        // `a_people_that_cannot_survive_winning_does_not_fight` binds it.
        let can_fight = pop * (1.0 - WAR_LOSS) >= VIABLE_MIN;
        let Some(home) = self.best_home(
            people,
            era,
            from,
            roller_strength(pop, offset, year),
            can_fight,
        ) else {
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
            // The resettle is tallied HERE, where it happens, rather than at
            // the top-level call site: a cascade's terminal roller reaches
            // vacant land exactly like a first-hop one does, and counting only
            // the top-level `cascade: 0` outcome under-reported the tally its
            // own doc comment describes (The Tumult, final review F-2).
            self.tally.resettled += 1;
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

    /// Whether a people takes the initiative at all — spec §4.2a's
    /// **disposition** inhibition, the durable one. A people whose authored
    /// `threat_response` (flee 0 ↔ stand 1) falls below `RAID_DISPOSITION_MIN`
    /// never raids, however strong it is on paper; a people with no authored
    /// disposition is not vetoed, so a bake handed no data behaves exactly as
    /// it did before the gate existed.
    ///
    /// This is the gate that makes raiding heterogeneous ACROSS peoples, and it
    /// buys an asymmetric aversion structure for free: A declines B while B
    /// raids A, because each people gates on its own trait and never on the
    /// pair. Like [`Bake::has_spoils`] it is a conjunct in both candidate
    /// loops — a timid people driven off its land pioneers rather than rolling
    /// over a holder it could have beaten.
    fn takes_the_initiative(&self, people: KindId) -> bool {
        match self.disposition.get(&people) {
            None => true,
            Some(&d) => d >= RAID_DISPOSITION_MIN,
        }
    }

    /// Whether a community is worth raiding at all — spec §4.2a's **no-spoils**
    /// inhibition, the momentary one. A community whose pressure has reached
    /// `NO_SPOILS_PRESSURE` is already consuming its cell's whole effective
    /// yield: there is no surplus to seize, so it is not a candidate however
    /// weak it is and however rich its ground. A cell the era has made
    /// worthless (`eff_capacity == 0.0`) has no spoils either, by the same
    /// reading.
    ///
    /// A veto, not a preference: it is a conjunct in both candidate loops (the
    /// seated raider's in [`Bake::maybe_raid`] and the roller's in
    /// [`Bake::best_home`]), so inhibitions compose without interacting. On the
    /// roller's side it is also what blocks the pathological regress of
    /// remnants preying on remnants all the way down.
    fn has_spoils(&self, era: &EraClimate, idx: usize) -> bool {
        let c = &self.communities[idx];
        self.eff_capacity(era, c.site) > 0.0 && self.pressure_of(idx, era) < NO_SPOILS_PRESSURE
    }

    /// A community's crowding pressure on its cell this era: population
    /// against effective capacity, scaled by per-capita need. Reads
    /// `population` only — `stores` must never enter this term, or a
    /// successful extractor would starve itself on its own tribute (spec
    /// §4.2a).
    fn pressure_of(&self, idx: usize, era: &EraClimate) -> f64 {
        let c = &self.communities[idx];
        let eff = self.eff_capacity(era, c.site);
        c.population * NEED / eff
    }

    /// A community's raiding strength: its population plus a weighted share
    /// of its stores, scaled by its tech horizon. Heterogeneous strength is
    /// the fuel of predation — equals do not prey on one another. Stores
    /// (walls, retainers, granaries) are strength the local land does not
    /// have to feed, via `STORE_WEIGHT`.
    fn strength(&self, idx: usize) -> f64 {
        let c = &self.communities[idx];
        (c.population + c.stores * STORE_WEIGHT) * tech_weight(c.tech)
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
            stores: 0.0,
        });
        // Keep the growth buffer exactly parallel to `communities`: a community
        // opened mid-epoch has grown nothing yet this epoch, and so owes
        // nothing if it is subordinated before the epoch closes.
        self.epoch_growth.push(0.0);
        self.node_index.insert(site, community_idx);
        self.tally.records_total += 1;
        community_idx
    }

    /// Close a community's record: mark it dead, stamp the ending, free its
    /// cell from the one-alive-per-site index, and dissolve every tribute
    /// relation it was party to.
    ///
    /// **Dissolution is a coherence floor, not a feature** (spec §4.4): a
    /// relation may not outlive either party, in either role — a dead
    /// subordinate pays nobody, and a dead patron collects from nobody. Both
    /// directions are removed here because `tribute` holds community *indices*,
    /// and an entry naming a closed community is a dangling index into
    /// `communities` — a silent corruption that would surface as a wrong
    /// collection (or a panic) on some unrelated seed. Freed subordinates do
    /// NOT cascade; the collapse-release avalanche is an explicit spec §9
    /// non-goal, distinct from this cleanup.
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
        // …and it is party to no relation, as subordinate or as patron.
        self.tribute.remove(&idx);
        self.tribute.retain(|_, t| t.patron != idx);
    }

    /// Open a new epoch: zero every community's growth buffer.
    ///
    /// A remittance is paid from *that epoch's* growth (spec §4.2), so the
    /// buffer must hold this epoch's increments and nothing else. Rebuilt by
    /// length rather than filled in place so it stays parallel to
    /// `communities` even if a caller ever appends by some other route.
    fn begin_epoch(&mut self) {
        self.epoch_growth.clear();
        self.epoch_growth.resize(self.communities.len(), 0.0);
    }

    /// Each patron collects from each of its subordinates: it demands what its
    /// assessment says (set from the cell it can SEE) and receives what the
    /// subordinate hands over — paid from **that epoch's growth**, never the
    /// standing stock, which is the difference between milking a community and
    /// killing it. A fully-taxed subordinate is left exactly where it began the
    /// epoch; it still grows in any epoch its patron under-assesses, just
    /// slower.
    ///
    /// The remittance lands in `stores`, never in `population` (spec §4.2a):
    /// tribute is wealth, not bodies, and a patron whose winnings entered the
    /// pressure term would starve itself on its own success.
    ///
    /// Deterministic: the relation table is a `BTreeMap`, so the pass runs in
    /// subordinate-index order, and it is snapshotted to a `Vec` first so no
    /// entry's outcome can depend on a mutation made earlier in the same pass.
    /// Every remittance reads only `assessment` (frozen when the relation
    /// formed) and `epoch_growth` (frozen by the step loop that has already
    /// finished), so no entry can read what an earlier one wrote and the order
    /// is immaterial today. That is a property of what this loop happens to
    /// read, not one anything enforces: a term that read the patron's `stores`
    /// — the obvious next reach, since `strength` does — would make the order
    /// decide the outcome, and the snapshot would then be load-bearing rather
    /// than belt-and-braces.
    fn collect_tribute(&mut self, year: f64) {
        let relations: Vec<(usize, Tribute)> = self.tribute.iter().map(|(&s, &t)| (s, t)).collect();
        for (sub, rel) in relations {
            // Cheap, and the failure it guards is silent. `close` dissolves
            // both directions of every relation a dying community was party to
            // (spec §4.4), so this should be unreachable — but a corpse taxed,
            // or a dead patron quietly enriched, would show up nowhere.
            if !self.communities[sub].alive || !self.communities[rel.patron].alive {
                continue;
            }
            let surplus = self.epoch_growth[sub].max(0.0);
            let remittance = rel.assessment.min(surplus);
            self.communities[sub].population -= remittance;
            self.communities[rel.patron].stores += remittance;
            self.tally.tribute_collected += remittance;
            // A no-op in today's call sequence — `grow` touched the patron at
            // this same `year` and this same population earlier in the epoch,
            // and collection moves `stores`, which `touch` does not read. Kept
            // for the same reason the subordination branch keeps its own
            // (`maybe_raid`): every mutation site in this bake records the
            // community it moved, and a future caller reaching collection by
            // another route must not silently skip that.
            self.touch(rel.patron, year);
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

        let pressure = self.pressure_of(idx, era);

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

    /// Opportunistic predation (The Tumult), now resolving **two outcomes**
    /// decided by the mobility of the prize (The Tithe, spec §4.1): a
    /// community raids the reachable occupied neighbour whose strength it can
    /// beat by `RAID_MARGIN` (dominance) and which still has a surplus worth
    /// taking (`has_spoils`) — decoupled from its own crowding. Predation is
    /// `motive × capability × inhibition` (spec §4.2a): the inhibitions are
    /// conjoined vetoes in this candidate loop, so they compose without
    /// interacting, and each can only ever *reduce* conflict. See
    /// [`Bake::has_spoils`].
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
    /// **Which outcome fires is the mobility of the prize** (spec §4.1):
    ///
    /// - `Spoil::Evict` — the target's cell is worth MORE this era
    ///   (covetousness). An immobile prize is takeable only by *occupying* it,
    ///   so the raid evicts and seizes. This is the shipped path, below,
    ///   unchanged.
    /// - `Spoil::Subordinate` — the cell is no better, but the target is
    ///   productive (`has_spoils`, i.e. it has growth headroom, which is the
    ///   inverse of the no-spoils veto and composes with it for free). Its
    ///   people and their product are a MOBILE prize, takeable repeatedly
    ///   without displacing anyone, so the raid imposes tribute and nobody
    ///   moves.
    ///
    /// The second branch is genuine new motive rather than a relabelling: the
    /// shipped rule `continue`d on `t_val <= raider_val`, so a strong community
    /// ignored a poorer neighbour outright. Under tribute a neighbour whose
    /// *land* is no prize but whose *people* are productive is worth milking,
    /// and a dominant grows **without changing cell** — the accumulation term
    /// The Tumult's sub-critical measurement said the model was missing. One
    /// scan finds the best target of either kind; there is no second pass.
    ///
    /// **Three skips guard the subordination branch**, all placed *after* the
    /// covet test so none of them can ever veto an eviction (they decide who
    /// may be milked, never who may be conquered):
    ///
    /// 1. **The raider is itself a subordinate** — a vassal takes no vassal.
    /// 2. **The target is itself a patron** — a patron is not subordinated.
    ///    Together with (1) these keep the patron set and the subordinate set
    ///    disjoint, which *is* spec §4.4's one-level-star invariant: keying
    ///    `tribute` by subordinate bounds out-degree to one, but a functional
    ///    graph still admits chains and cycles, and measurement without these
    ///    two found most standing relations sitting under a patron who was
    ///    themselves paying someone.
    /// 3. **The target already pays THIS raider** — nothing further to take
    ///    from it this epoch.
    ///
    /// A target paying *someone else* is a live candidate: a second bid
    /// **transfers** the patronage, and the old patron does not contest
    /// (contesting is the deferred protection lever, spec §9). But the bid
    /// must clear `RAID_MARGIN` over the **incumbent patron**, not merely over
    /// the subordinate (spec §4.4's hysteresis) — without that bound, rival
    /// patrons swap the same targets back and forth every epoch (~87% churn,
    /// measured), and neither a store nor an adaptive assessment can build any
    /// history on a relation whose collector changes each epoch. All of this
    /// is stated in the spec and honoured here precisely so the bake's
    /// iteration order cannot decide it silently.
    ///
    /// On the eviction branch the outcome is **conquest of immobile land**, not
    /// plunder (spec §4.3).
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
    /// tie-broken by eviction over subordination (see [`Spoil::rank`]), then
    /// the weakest, then the lowest `CellId` (`f64::total_cmp`
    /// throughout), and never touches the epoch stream — `maybe_raid` itself
    /// consumes no draw. It does change which communities exist and how
    /// pressured they are, so the *sequence* of `grow`'s `DAUGHTER_PROB` draws
    /// downstream does move; that is the genesis epoch spec §7 declares, not a
    /// break in byte-identity for a fixed physics.
    fn maybe_raid(&mut self, raider: usize, era: &EraClimate, year: f64) {
        let raider_site = self.communities[raider].site;
        // The durable inhibition: a timid people never takes the initiative,
        // so it never enters the candidate loop at all.
        if !self.takes_the_initiative(self.records[self.communities[raider].record].people) {
            return;
        }
        // Too small to seat itself after the war it is contemplating: decline,
        // before any tally moves (see the `VIABLE_MIN` note above).
        if self.communities[raider].population * (1.0 - WAR_LOSS) < VIABLE_MIN {
            return;
        }
        let raider_str = self.strength(raider);
        let raider_val = self.eff_capacity(era, raider_site);
        // Spec §4.4, guard (1): a community that is itself paying someone takes
        // no vassal of its own. Hoisted because it does not vary over the
        // candidate walk — but applied per-candidate, inside the else-chain,
        // so a vassal can still *evict*.
        let raider_is_vassal = self.tribute.contains_key(&raider);
        // (target index, that cell's value, the target's strength, its cell,
        //  and how a raid on it would resolve)
        let mut best: Option<(usize, f64, f64, CellId, Spoil)> = None;
        for n in traversable_neighbors(self.cur(), raider_site) {
            let Some(&t) = self.node_index.get(&n) else {
                continue;
            };
            let t_val = self.eff_capacity(era, n);
            let t_str = self.strength(t);
            if raider_str <= t_str * RAID_MARGIN {
                continue; // dominance: only a fight it can win
            }
            if !self.has_spoils(era, t) {
                continue; // inhibition: a starving neighbour is a husk (spec §4.2a)
            }
            // Classification, not a veto: the mobility of the prize decides
            // how a raid on this neighbour would resolve (spec §4.1).
            let kind = if t_val > raider_val {
                Spoil::Evict // land that is BETTER *this era*: take the ground
            } else if raider_is_vassal {
                continue; // (1) a vassal takes no vassal — no depth (spec §4.4)
            } else if self.tribute.values().any(|tr| tr.patron == t) {
                continue; // (2) a patron is not subordinated — no depth either
            } else if let Some(tr) = self.tribute.get(&t) {
                // The target already pays someone.
                //
                // (3) Already ours: nothing further to take this epoch. Stated
                // explicitly because it is a rule, though the hysteresis just
                // below independently subsumes it — no community can clear
                // `RAID_MARGIN > 1` over its own strength, so a raider can
                // never out-bid itself. Deleting this line alone changes no
                // behaviour; deleting both re-takes one's own subordinate every
                // scan (mutation-verified in that direction).
                if tr.patron == raider {
                    continue;
                }
                // A takeover, and it must out-muscle the INCUMBENT, not the
                // subordinate: hysteresis (spec §4.4). The incumbent still does
                // not fight — it simply keeps what a lesser rival cannot take.
                if raider_str <= self.strength(tr.patron) * RAID_MARGIN {
                    continue;
                }
                Spoil::Subordinate // a takeover that plainly out-muscles the incumbent
            } else {
                Spoil::Subordinate // no better land, but productive people
            };
            let better = match best {
                None => true,
                Some((_, bv, bs, bc, bk)) => t_val
                    .total_cmp(&bv) // the MOST valuable land
                    .then(kind.rank().cmp(&bk.rank())) // at equal value, EVICTION
                    .then(bs.total_cmp(&t_str)) // then the WEAKEST
                    .then(bc.cmp(&n)) // then the lowest CellId
                    .is_gt(),
            };
            if better {
                best = Some((t, t_val, t_str, n, kind));
            }
        }
        let Some((target, _, _, _, kind)) = best else {
            return; // nothing worth taking, or nothing beatable
        };
        let prize = self.communities[target].site;

        // Exhaustive, so a third kind of prize cannot silently inherit the
        // eviction path. `Evict` falls through to the shipped body below
        // rather than being nested into an arm: its `close`/`open` sequencing
        // is load-bearing for the one-alive-per-site invariant, so that body
        // is left byte-for-byte as it was, not even re-indented. (`prize` is
        // read by both outcomes, so its binding — and it alone — was hoisted
        // above the `match`; same expression, same position in the sequence,
        // and nothing between it and its old site can observe the move.)
        match kind {
            Spoil::Subordinate => {
                // The mobile prize: the target keeps its cell, its people and
                // its life, and begins paying. NOTHING here touches
                // `node_index` — subordination moves nobody, so the
                // one-alive-per-site invariant is untouched by construction
                // rather than by careful sequencing.
                //
                // The assessment reads the SUBORDINATE'S CELL, never its
                // granary: land tax is assessed on area precisely because the
                // granary cannot be seen (spec §4.2). A second bid overwrites,
                // which IS the patronage transfer — `tribute` is keyed by the
                // subordinate, so one patron per community is structural.
                let cap = self.eff_capacity(era, prize);
                let assessment = (cap * ASSESS_RATE).clamp(0.0, cap * ASSESS_MAX);
                let previous = self.tribute.insert(
                    target,
                    Tribute {
                        patron: raider,
                        assessment,
                        since: year,
                    },
                );
                // The one-level-star invariant, checked where it is ESTABLISHED
                // rather than at the end of the bake: a chain that formed and
                // dissolved mid-span would be invisible to any closing reading.
                // Holds inductively — the patron set and the subordinate set
                // are kept disjoint by the two guards in the classification
                // above (spec §4.4).
                debug_assert!(
                    !self.tribute.contains_key(&raider),
                    "relation depth: a vassal ({raider}) took a vassal ({target})"
                );
                debug_assert!(
                    !self.tribute.values().any(|tr| tr.patron == target),
                    "relation depth: a patron ({target}) was subordinated to {raider}"
                );
                // Flows, counted apart: a takeover subjugates nobody new, and
                // pooling the two would let churn read as volume (spec §8.1).
                match previous {
                    Some(_) => self.tally.patronage_transfers += 1,
                    None => self.tally.subordinations_formed += 1,
                }
                // A no-op in today's call sequence — `step_community` calls
                // `grow` (which touches the raider at this same `year`, at this
                // same population) immediately before `maybe_raid`, and `touch`
                // is idempotent. Kept so the branch mirrors the eviction path
                // and so a future caller reaching `maybe_raid` by another route
                // still records the raider's peak; it is deliberately the one
                // line here that COULD have moved a committed record, and it
                // provably does not (seed 42's records are byte-identical).
                self.touch(raider, year);
                return;
            }
            Spoil::Evict => {}
        }

        self.tally.raided += 1;
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
            // `resettled` is tallied inside `relocate`, at the vacant-land
            // branch itself, so every hop that reaches vacant ground counts —
            // including a cascade's terminal one. Nothing to add here.
            Relocation::Settled { cascade: 0 } => {}
            Relocation::Settled { cascade } => self.tally.record_cascade(cascade),
            Relocation::Lost => self.tally.collapsed += 1,
        }
    }

    /// A comfortable community grows logistically, and — if very comfortable —
    /// may throw off a daughter onto a vacant habitable neighbour. Also the
    /// one place `STORE_DECAY` applies. Precisely: `grow` has a single call
    /// site and runs at most once per community per epoch, and only for a
    /// community that survived its own turn — one evicted by climate or lost
    /// to Famine returns from `step_community` before reaching here, and its
    /// stores are moot because they are lost on closure anyway. So every
    /// community that survives into the next epoch has decayed exactly once,
    /// and no hoard can double-decay.
    fn grow(&mut self, idx: usize, era: &EraClimate, year: f64, pressure: f64) {
        let c = &mut self.communities[idx];
        let before = c.population;
        c.population *= 1.0 + GROWTH_RATE * (1.0 - pressure);
        // The increment ACTUALLY applied — the only thing tribute may be paid
        // out of (spec §4.2). Negative above unit pressure, where a crowded
        // community shrinks; [`Bake::collect_tribute`] floors it at zero, so a
        // shrinking subordinate simply owes nothing rather than paying a
        // negative tribute back to its patron.
        let increment = c.population - before;
        c.stores *= STORE_DECAY;
        // ACCUMULATE, never overwrite: the buffer's meaning is "this epoch's
        // growth", not "the last growth call's increment". `grow` has one call
        // site and runs at most once per community per epoch today, so `+=` and
        // `=` are the same arithmetic on every world this bake produces (seed 42
        // is byte-identical either way). They differ only if that ever stops
        // holding, and then `=` would silently discard the earlier increment —
        // an under-collection with no symptom. `begin_epoch` zeroes the buffer,
        // so nothing carries across epochs.
        self.epoch_growth[idx] += increment;
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
        disposition: &cfg.disposition,
        records: Vec::new(),
        communities: Vec::new(),
        node_index: BTreeMap::new(),
        next_id: 1,
        stream: seed.derive(hornvale_history::streams::BAKE).stream(),
        tribute: BTreeMap::new(),
        epoch_growth: Vec::new(),
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
        // Last epoch's increments are spent: nothing may be taxed twice.
        bake.begin_epoch();
        let snapshot: Vec<usize> = (0..bake.communities.len())
            .filter(|&i| bake.communities[i].alive)
            .collect();
        for idx in snapshot {
            bake.step_community(idx, &era, year);
        }
        // Tribute is collected once the whole world has stepped, so there is
        // growth to tax and so no subordinate's remittance depends on whether
        // its patron happened to be stepped before or after it.
        bake.collect_tribute(year);
        year += cfg.epoch_years;
    }

    // 3. Close at `now`: alive records keep `ended = None`.
    let now = cfg.end_year;
    bake.tally.alive_at_now = bake.records.iter().filter(|r| r.is_alive()).count() as u64;
    // The stock at `now`: how many relations survived the span, and the widest
    // star among them (spec §8.2's runaway-hub reading). `close` dissolves both
    // directions of a relation, so every entry left here must name two live
    // communities — asserted rather than assumed, over a REAL bake, because a
    // dangling index is exactly the corruption that stays silent until it
    // panics on some unrelated seed. Counted in `BTreeMap` key order — a
    // maximum is order-free anyway, but the container is never a hash map.
    bake.tally.tribute_relations_at_now = bake.tribute.len() as u64;
    let mut per_patron: BTreeMap<usize, u64> = BTreeMap::new();
    for (&sub, t) in &bake.tribute {
        debug_assert!(
            bake.communities[sub].alive && bake.communities[t.patron].alive,
            "a standing tribute relation must name two ALIVE communities: \
             {sub} (alive {}) pays {} (alive {})",
            bake.communities[sub].alive,
            t.patron,
            bake.communities[t.patron].alive,
        );
        *per_patron.entry(t.patron).or_insert(0) += 1;
    }
    bake.tally.max_subordinates = per_patron.values().copied().max().unwrap_or(0);
    // The accumulator readout (spec §8.2): the biggest hoard still standing.
    // Alive communities only — a store dies with its holder — and `total_cmp`,
    // never `>`, so the fold is total and deterministic.
    bake.tally.max_stores_at_now = bake
        .communities
        .iter()
        .filter(|c| c.alive)
        .map(|c| c.stores)
        .fold(0.0f64, |a, b| if a.total_cmp(&b).is_lt() { b } else { a });
    // The relations that survived the whole span, translated out of bake
    // indices into durable community handles (the same translation `ended-by`
    // and `founded-from` rely on) so emission never has to know what an index
    // meant. `BTreeMap` iteration, so subordinate order.
    let tribute: Vec<TributeRelation> = bake
        .tribute
        .iter()
        .map(|(&sub, t)| TributeRelation {
            subordinate: bake.communities[sub].id,
            patron: bake.communities[t.patron].id,
            since: t.since,
        })
        .collect();

    History {
        records: bake.records,
        now,
        tribute,
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
            disposition: no_disposition(),
            records: Vec::new(),
            communities: Vec::new(),
            node_index: BTreeMap::new(),
            next_id: 1,
            stream: Seed(1).derive(hornvale_history::streams::BAKE).stream(),
            tribute: BTreeMap::new(),
            epoch_growth: Vec::new(),
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
    /// `capacity_of` paints the value gradient the conflict tests need.
    fn cascade_world(
        capacity_of: impl Fn(CellId) -> f64,
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
        let capacity = CellMap::from_fn(&geo, capacity_of);
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

    /// The disposition map a hand-built [`Bake`] uses when the test is not
    /// about disposition: empty, so nobody is vetoed (the same fail-open the
    /// composition root sees when a people carries no psyche).
    fn no_disposition() -> &'static BTreeMap<KindId, f64> {
        static NONE: BTreeMap<KindId, f64> = BTreeMap::new();
        &NONE
    }

    /// A hand-built [`Bake`] over [`cascade_world`]'s inputs, with an empty
    /// record set and a fixed stream.
    fn hand_bake<'a>(
        graphs: &'a [ConnectionGraph],
        capacity: &'a CellMap<f64>,
        river_prox: &'a CellMap<f64>,
        refugia: &'a CellMap<bool>,
        disposition: &'a BTreeMap<KindId, f64>,
    ) -> Bake<'a> {
        Bake {
            graphs,
            cur_graph: 0,
            capacity,
            river_prox,
            refugia,
            disposition,
            records: Vec::new(),
            communities: Vec::new(),
            node_index: BTreeMap::new(),
            next_id: 1,
            stream: Seed(1).derive(hornvale_history::streams::BAKE).stream(),
            tribute: BTreeMap::new(),
            epoch_growth: Vec::new(),
            tally: BakeCensus::default(),
        }
    }

    /// A uniform, fully habitable [`EraClimate`] at `day` over a one-cell
    /// world — the frame the store test's fixture is built on.
    fn era_at(day: f64) -> EraClimate {
        use hornvale_kernel::ReferenceElevation;
        let geo = Geosphere::new(1);
        EraClimate {
            day,
            ice: CellMap::from_fn(&geo, |_| false),
            habitable: CellMap::from_fn(&geo, |_| true),
            sea_level: ReferenceElevation::new(0.0).unwrap(),
            ice_fraction: 0.0,
        }
    }

    #[test]
    fn stores_raise_strength_but_never_pressure() {
        // A hand-built Bake with one genesis community (population 10) on a
        // one-cell, fully-habitable world, built from owned locals the test
        // borrows — the file's own fixture idiom (cf. `cascade_world`), not a
        // leaked `'static`. Give it stores and confirm:
        //   (a) strength rises with stores
        //   (b) the pressure the bake computes is unchanged
        let geo = Geosphere::new(1);
        let graphs = vec![full_land_graph(&geo)];
        let capacity = CellMap::from_fn(&geo, |_| 100.0);
        let river_prox = CellMap::from_fn(&geo, |_| 0.0);
        let refugia = CellMap::from_fn(&geo, |_| false);
        let mut bake = hand_bake(&graphs, &capacity, &river_prox, &refugia, no_disposition());
        bake.open(
            KindId("goblin"),
            CellId(0),
            0.0,
            10.0,
            Founding::Genesis(CellId(0)),
            None,
            0.0,
        );

        let before_strength = bake.strength(0);
        let before_pressure = bake.pressure_of(0, &era_at(0.0));

        bake.communities[0].stores = 100.0;

        let after_strength = bake.strength(0);
        let after_pressure = bake.pressure_of(0, &era_at(0.0));

        assert!(
            after_strength > before_strength,
            "stores must feed strength: {before_strength} -> {after_strength}"
        );
        assert_eq!(
            before_pressure.to_bits(),
            after_pressure.to_bits(),
            "stores must NOT feed pressure — a successful extractor would starve itself"
        );
    }

    /// A patron on prime land beside a subordinate on prime land, with the
    /// relation already standing and its assessment set far above anything the
    /// subordinate's land could produce. The oversized assessment is what makes
    /// the collection tests *discriminating*: under spec §4.2's rule the
    /// remittance is then exactly the epoch's growth increment, and under the
    /// wrong rule (taxing the standing stock) it would be the whole population.
    fn tribute_pair<'a>(
        geo: &Geosphere,
        graphs: &'a [ConnectionGraph],
        capacity: &'a CellMap<f64>,
        river_prox: &'a CellMap<f64>,
        refugia: &'a CellMap<bool>,
    ) -> (Bake<'a>, usize, usize) {
        let mut bake = hand_bake(graphs, capacity, river_prox, refugia, no_disposition());
        let far = geo.neighbors(CellId(0))[0];
        let patron = bake.open(
            KindId("goblin"),
            CellId(0),
            0.0,
            40.0,
            Founding::Genesis(CellId(0)),
            None,
            0.0,
        );
        let sub = bake.open(
            KindId("kobold"),
            far,
            0.0,
            10.0,
            Founding::Genesis(far),
            None,
            0.0,
        );
        bake.tribute.insert(
            sub,
            Tribute {
                patron,
                assessment: 1.0e9,
                since: 0.0,
            },
        );
        (bake, patron, sub)
    }

    #[test]
    fn a_patron_accumulates_stores_without_its_pressure_rising() {
        // Spec §4.2 + §4.2a, the two halves of the slice's central claim:
        //   (a) the remittance is paid out of THIS epoch's growth increment,
        //       never the standing stock — so the subordinate is milked back to
        //       exactly where it began the epoch, and no further;
        //   (b) it lands in the patron's `stores`, never its `population`, so
        //       the patron's crowding pressure is bit-for-bit unmoved. A
        //       successful extractor that fed its winnings into `population`
        //       would drive itself into Famine, and the readout would report
        //       "accumulation does not chain" when the truth was self-harm.
        let (geo, graphs, capacity, river_prox, refugia, era) = cascade_world(|_| RICH);
        let (mut bake, patron, sub) = tribute_pair(&geo, &graphs, &capacity, &river_prox, &refugia);

        bake.begin_epoch();
        let sub_before_growth = bake.communities[sub].population;
        let pressure = bake.pressure_of(sub, &era);
        bake.grow(sub, &era, 0.0, pressure);
        let increment = bake.communities[sub].population - sub_before_growth;
        assert!(
            increment > 0.0,
            "precondition: the subordinate must actually have grown this epoch"
        );

        let patron_pressure_before = bake.pressure_of(patron, &era);
        let patron_population_before = bake.communities[patron].population;
        bake.collect_tribute(0.0);

        assert_eq!(
            bake.communities[patron].stores.to_bits(),
            increment.to_bits(),
            "the patron must receive exactly this epoch's growth increment \
             ({increment}), not the standing stock ({sub_before_growth}): got {}",
            bake.communities[patron].stores
        );
        assert_eq!(
            bake.communities[patron].population.to_bits(),
            patron_population_before.to_bits(),
            "tribute is wealth, not bodies: the patron's population must not move"
        );
        assert_eq!(
            bake.pressure_of(patron, &era).to_bits(),
            patron_pressure_before.to_bits(),
            "stores must never enter the pressure term (spec §4.2a)"
        );
        assert!(
            (bake.communities[sub].population - sub_before_growth).abs() < 1.0e-9,
            "a fully-taxed subordinate is milked back to exactly where it \
             started the epoch, never below it: {} vs {sub_before_growth}",
            bake.communities[sub].population
        );
        assert_eq!(
            bake.tally.tribute_collected.to_bits(),
            increment.to_bits(),
            "the run total must count what actually moved"
        );
    }

    #[test]
    fn last_epochs_growth_is_never_taxed_twice() {
        // The growth buffer is strictly per-epoch (spec §4.2's "that epoch's
        // growth"). A stale increment left standing would be re-taxed every
        // epoch forever — the same surplus milked repeatedly out of a community
        // that never grew again, which is the standing stock by another name.
        // This is the one test that catches a missing `begin_epoch`; the
        // growth-vs-stock discrimination itself is asserted next door, in
        // `a_patron_accumulates_stores_without_its_pressure_rising`.
        //
        // The between-epochs survival check below is load-bearing rather than
        // decorative: a rule taxing the stock drains this subordinate to zero
        // in the FIRST epoch, after which "no growth ⇒ no tribute" would hold
        // vacuously and the second epoch would prove nothing at all.
        let (geo, graphs, capacity, river_prox, refugia, era) = cascade_world(|_| RICH);
        let (mut bake, patron, sub) = tribute_pair(&geo, &graphs, &capacity, &river_prox, &refugia);

        bake.begin_epoch();
        let pressure = bake.pressure_of(sub, &era);
        bake.grow(sub, &era, 0.0, pressure);
        bake.collect_tribute(0.0);
        let stores_after_one = bake.communities[patron].stores;
        let sub_after_one = bake.communities[sub].population;
        assert!(
            stores_after_one > 0.0,
            "precondition: the first epoch must have collected something"
        );
        assert!(
            sub_after_one > 0.0,
            "precondition: a milked subordinate is still standing after the \
             epoch it was milked in — one drained to nothing makes the second \
             epoch's reading vacuous (spec §8.3)"
        );

        // A bad year: the subordinate does not grow at all.
        bake.begin_epoch();
        bake.collect_tribute(25.0);

        assert_eq!(
            bake.communities[patron].stores.to_bits(),
            stores_after_one.to_bits(),
            "an epoch with no growth yields no tribute: {} vs {stores_after_one}",
            bake.communities[patron].stores
        );
        assert_eq!(
            bake.communities[sub].population.to_bits(),
            sub_after_one.to_bits(),
            "a subordinate that did not grow must not be taxed on its stock"
        );
    }

    #[test]
    fn a_relation_naming_a_dead_community_collects_nothing() {
        // The coherence guard (spec §4.4): `close` dissolves every relation the
        // dead community was party to, so this is unreachable through the bake
        // — but the failure it prevents is silent (a dead patron quietly
        // enriched, or a corpse taxed), so it is asserted rather than assumed.
        let (geo, graphs, capacity, river_prox, refugia, era) = cascade_world(|_| RICH);
        let (mut bake, patron, sub) = tribute_pair(&geo, &graphs, &capacity, &river_prox, &refugia);

        bake.begin_epoch();
        let pressure = bake.pressure_of(sub, &era);
        bake.grow(sub, &era, 0.0, pressure);
        // Kill the patron WITHOUT going through `close`, so the relation is
        // left dangling exactly as a missed cleanup would leave it.
        bake.communities[patron].alive = false;
        bake.collect_tribute(0.0);

        assert_eq!(
            bake.communities[patron].stores.to_bits(),
            0.0f64.to_bits(),
            "a dead patron collects from nobody"
        );
        assert_eq!(
            bake.tally.tribute_collected.to_bits(),
            0.0f64.to_bits(),
            "and nothing is tallied as having moved"
        );

        // The OTHER half of the same guard, on its own pair: a corpse is not
        // taxed either. Asserted separately because the two halves are separate
        // conditions — with only the patron case above, deleting the
        // subordinate half of the guard leaves the suite green while a dead
        // community pays tribute to a live patron.
        let (mut bake, patron, sub) = tribute_pair(&geo, &graphs, &capacity, &river_prox, &refugia);
        bake.begin_epoch();
        let pressure = bake.pressure_of(sub, &era);
        bake.grow(sub, &era, 0.0, pressure);
        let corpse_population = bake.communities[sub].population;
        bake.communities[sub].alive = false;
        bake.collect_tribute(0.0);

        assert_eq!(
            bake.communities[patron].stores.to_bits(),
            0.0f64.to_bits(),
            "a corpse pays nobody: the patron's store must stay empty"
        );
        assert_eq!(
            bake.communities[sub].population.to_bits(),
            corpse_population.to_bits(),
            "and nothing is taken off the corpse itself"
        );
        assert_eq!(
            bake.tally.tribute_collected.to_bits(),
            0.0f64.to_bits(),
            "and nothing is tallied as having moved"
        );
    }

    #[test]
    fn no_subordinate_ends_an_epoch_below_where_it_began_it() {
        // Spec §8.3's survival claim — tribute MILKS, it never kills — stated
        // as something a defect can actually redden.
        //
        // The census-level headcount that used to carry this claim
        // (`alive_at_now == records_total`, over the integration fixture)
        // cannot: starvation needs `population >= COLLAPSE_PRESSURE *
        // capacity`, and the logistic growth term is bounded BY capacity, so
        // in a quiet world nobody can starve however hard they are farmed —
        // and a subordinate drained to zero does not die, it sits there alive
        // at zero. The claim only has teeth against the per-subordinate
        // population BETWEEN epochs, which `bake()` never exposes.
        //
        // So this drives the bake's own epoch loop by hand — `begin_epoch`,
        // every alive community through `step_community`, then
        // `collect_tribute` — over a value-flat world: uniform capacity, so no
        // cell is ever worth more than its neighbour, every raid the real rule
        // resolves is a subordination, and (asserted below) no war, eviction or
        // famine fires. `grow` and `collect_tribute` are then the ONLY two
        // things that move a population, so the floor is attributable to
        // tribute alone.
        let (geo, graphs, capacity, river_prox, refugia, era) = cascade_world(|_| RICH);
        let mut bake = hand_bake(&graphs, &capacity, &river_prox, &refugia, no_disposition());
        // One strong community ringed by weaker, productive ones: it out-muscles
        // each of them four-fold (clear of `RAID_MARGIN`), and their land is no
        // better than its own, so the only prize on offer is their product.
        bake.open(
            KindId("goblin"),
            CellId(0),
            0.0,
            40.0,
            Founding::Genesis(CellId(0)),
            None,
            0.0,
        );
        for &n in geo.neighbors(CellId(0)) {
            bake.open(
                KindId("kobold"),
                n,
                0.0,
                10.0,
                Founding::Genesis(n),
                None,
                0.0,
            );
        }

        /// Epochs driven — long enough that relations form early and then
        /// collect for most of the run.
        const EPOCHS: usize = 20;
        /// Years per driven epoch (the bake's own default step).
        const EPOCH_YEARS: f64 = 25.0;
        // Slack for the float round-trip of `(p + increment) - increment`; the
        // floor is otherwise touched exactly, see `taxed_to_the_floor`.
        const EPS: f64 = 1.0e-9;
        let mut floor_checks = 0_u32;
        let mut milked_epochs = 0_u32;
        let mut taxed_to_the_floor = 0_u32;
        for epoch in 0..EPOCHS {
            let year = epoch as f64 * EPOCH_YEARS;
            bake.begin_epoch();
            let began: Vec<f64> = bake.communities.iter().map(|c| c.population).collect();
            let alive: Vec<usize> = (0..bake.communities.len())
                .filter(|&i| bake.communities[i].alive)
                .collect();
            for idx in alive {
                bake.step_community(idx, &era, year);
            }
            // Who owes as collection begins, and how much has moved so far.
            let owing: Vec<usize> = bake.tribute.keys().copied().collect();
            let before_collection = bake.tally.tribute_collected;
            bake.collect_tribute(year);
            if bake.tally.tribute_collected > before_collection {
                milked_epochs += 1;
            }
            for sub in owing {
                // A community opened DURING this epoch (a daughter) has no
                // epoch-start population to compare against — and grew nothing,
                // so it owed nothing either.
                let Some(&start) = began.get(sub) else {
                    continue;
                };
                let ended = bake.communities[sub].population;
                assert!(
                    ended >= start - EPS,
                    "epoch {epoch}: subordinate {sub} ended at {ended}, BELOW the {start} \
                     it began the epoch at. A remittance is capped by that epoch's growth \
                     (spec §4.2), so tribute may take a community's whole increment and \
                     never one head of its standing stock."
                );
                if (ended - start).abs() < EPS {
                    taxed_to_the_floor += 1;
                }
                floor_checks += 1;
            }
        }

        // Anti-vacuity: the floor above is worthless unless relations formed,
        // wealth actually moved along them, and the bound was TIGHT — a
        // subordinate milked of its whole increment sits exactly ON the floor,
        // which is what makes "one head below" a reddening difference rather
        // than slack absorbed by an unspent margin.
        assert!(
            bake.tally.subordinations_formed > 0,
            "precondition: a relation must form before any floor means anything"
        );
        assert!(
            floor_checks > 0,
            "precondition: the floor must have been read at least once"
        );
        assert!(
            milked_epochs > 0,
            "precondition: tribute must actually have flowed in some epoch \
             (collected {})",
            bake.tally.tribute_collected
        );
        assert!(
            taxed_to_the_floor > 0,
            "precondition: some subordinate must have been milked of its WHOLE \
             increment, or the floor is never touched and never discriminating \
             ({floor_checks} readings, none tight)"
        );
        // Attributability: nothing else in this world moves a population.
        assert_eq!(
            bake.tally.raided, 0,
            "value-flat world: no eviction, so no war loss can be mistaken for tribute"
        );
        assert_eq!(
            bake.tally.collapsed, 0,
            "nobody starves here: a famine death would confound the floor"
        );
        assert_eq!(
            bake.tally.migrated, 0,
            "no cell turns hostile here: a migration would confound the floor"
        );
    }

    #[test]
    fn subordination_leaves_both_communities_exactly_where_they_stand() {
        // The mobile prize (spec §4.1): the raid takes the people's product,
        // not their ground, so nobody moves, nobody dies, and — the invariant
        // that matters — `node_index` is not touched at all. A subordination
        // that quietly re-indexed a cell would break the one-alive-per-site
        // invariant in a way no census field would show.
        let (geo, graphs, capacity, river_prox, refugia, era) = cascade_world(|_| RICH);
        let mut bake = hand_bake(&graphs, &capacity, &river_prox, &refugia, no_disposition());
        let far = geo.neighbors(CellId(0))[0];

        // Equal-value cells (the world is value-flat), so `t_val > raider_val`
        // is FALSE and the shipped eviction path cannot fire. A big raider, a
        // small but far-from-capacity — i.e. productive — neighbour.
        let raider = bake.open(
            KindId("goblin"),
            CellId(0),
            0.0,
            80.0,
            Founding::Genesis(CellId(0)),
            None,
            0.0,
        );
        let target = bake.open(
            KindId("kobold"),
            far,
            0.0,
            10.0,
            Founding::Genesis(far),
            None,
            0.0,
        );
        let index_before = bake.node_index.clone();

        bake.maybe_raid(raider, &era, 0.0);

        let t = bake
            .tribute
            .get(&target)
            .copied()
            .expect("a relation formed");
        assert_eq!(t.patron, raider, "the raider must be the patron");
        // Pinned exactly, not to a slack band: `ASSESS_RATE` of the
        // SUBORDINATE cell's effective capacity, which here is `RICH` (the
        // era mask is 1.0 everywhere and the clamp does not bind). A band of
        // `(0, RICH × ASSESS_MAX]` would pass for any rate in `(0, 0.5]`.
        // Which cell is read is bound by
        // `the_assessment_reads_the_subordinates_cell_not_the_raiders`, since
        // this world is value-flat and cannot distinguish the two.
        assert_eq!(
            t.assessment.to_bits(),
            (RICH * ASSESS_RATE).to_bits(),
            "assessment must be exactly eff_capacity × ASSESS_RATE: {}",
            t.assessment
        );
        assert_eq!(
            bake.tally.subordinations_formed, 1,
            "one first-time relation formed"
        );
        assert_eq!(
            bake.tally.patronage_transfers, 0,
            "nothing changed hands: the target was paying nobody"
        );
        assert_eq!(bake.tally.raided, 0, "equal-value land: no eviction");
        assert_eq!(bake.tally.fled, 0, "nobody was driven off");
        assert_eq!(
            bake.node_index, index_before,
            "subordination must not touch the one-alive-per-site index"
        );
        assert!(
            bake.communities[raider].alive && bake.communities[target].alive,
            "both communities must survive a subordination"
        );
        assert_eq!(bake.communities[raider].site, CellId(0), "raider stays put");
        assert_eq!(bake.communities[target].site, far, "subordinate stays put");
        assert_eq!(
            bake.communities[target].population.to_bits(),
            10.0f64.to_bits(),
            "subordination is not a war: no population is destroyed"
        );
    }

    /// Three cells in a line — `x ~ y ~ z`, with `x` and `z` **not** adjacent
    /// — deterministically chosen (lowest ids first) and asserted rather than
    /// hoped for: on a triangle every community would see every other and
    /// neither depth guard could be exercised in isolation.
    fn line_of_three(geo: &Geosphere) -> (CellId, CellId, CellId) {
        let x = CellId(0);
        for &y in geo.neighbors(x) {
            for &z in geo.neighbors(y) {
                if z != x && !geo.neighbors(x).contains(&z) {
                    return (x, y, z);
                }
            }
        }
        panic!("Geosphere::new(1) must offer an x~y~z with x and z non-adjacent");
    }

    /// Every standing relation's patron pays nobody — the one-level-star
    /// invariant, stated as the property the depth guards exist to hold. A
    /// chain `a → b → c` puts `b` in the table both as a key and as a patron,
    /// which is exactly what this rejects.
    fn assert_no_chained_relations(bake: &Bake<'_>) {
        for (sub, t) in &bake.tribute {
            assert!(
                !bake.tribute.contains_key(&t.patron),
                "relation depth: {sub} pays {}, who is themselves paying {:?}",
                t.patron,
                bake.tribute.get(&t.patron).map(|p| p.patron)
            );
        }
    }

    #[test]
    fn a_vassal_takes_no_vassal_of_its_own() {
        // Spec §4.4's first depth guard. Keying `tribute` by subordinate bounds
        // OUT-degree to one, which is a functional graph — it still admits
        // chains. A ~ B ~ C in a line, strongest to weakest: A subordinates B,
        // and B must then decline C, which it could otherwise plainly beat.
        let (geo, graphs, capacity, river_prox, refugia, era) = cascade_world(|_| RICH);
        let mut bake = hand_bake(&graphs, &capacity, &river_prox, &refugia, no_disposition());
        let (x, y, z) = line_of_three(&geo);

        let a = bake.open(
            KindId("goblin"),
            x,
            0.0,
            80.0,
            Founding::Genesis(x),
            None,
            0.0,
        );
        let b = bake.open(
            KindId("kobold"),
            y,
            0.0,
            30.0,
            Founding::Genesis(y),
            None,
            0.0,
        );
        let c = bake.open(
            KindId("bugbear"),
            z,
            0.0,
            10.0,
            Founding::Genesis(z),
            None,
            0.0,
        );

        // A's only occupied neighbour is B (C is out of reach), and 80 clears
        // 30 × RAID_MARGIN.
        bake.maybe_raid(a, &era, 0.0);
        assert_eq!(
            bake.tribute.get(&b).map(|t| t.patron),
            Some(a),
            "precondition: A must hold B, or the guard under test is never reached"
        );
        // B could beat C (30 > 10 × RAID_MARGIN) and C is productive — the ONLY
        // thing standing between them is B's own subjection.
        bake.maybe_raid(b, &era, 0.0);
        assert!(
            !bake.tribute.contains_key(&c),
            "a vassal must take no vassal: B pays A, so C must stay free"
        );
        assert_eq!(bake.tribute.len(), 1, "exactly one relation may stand");
        assert_no_chained_relations(&bake);
        assert_eq!(
            bake.tally.subordinations_formed, 1,
            "only A's bid may have formed anything"
        );
    }

    #[test]
    fn a_patron_is_not_itself_subordinated() {
        // Spec §4.4's second depth guard — the same line, raided from the other
        // end. B takes C first; A may then NOT take B, because a chain
        // A → B → C is the depth spec §5 preregisters the headline on the
        // absence of. Both guards are required: this one is unreachable in the
        // test above and that one is unreachable here.
        let (geo, graphs, capacity, river_prox, refugia, era) = cascade_world(|_| RICH);
        let mut bake = hand_bake(&graphs, &capacity, &river_prox, &refugia, no_disposition());
        let (x, y, z) = line_of_three(&geo);

        let a = bake.open(
            KindId("goblin"),
            x,
            0.0,
            80.0,
            Founding::Genesis(x),
            None,
            0.0,
        );
        let b = bake.open(
            KindId("kobold"),
            y,
            0.0,
            30.0,
            Founding::Genesis(y),
            None,
            0.0,
        );
        let c = bake.open(
            KindId("bugbear"),
            z,
            0.0,
            10.0,
            Founding::Genesis(z),
            None,
            0.0,
        );

        bake.maybe_raid(b, &era, 0.0);
        assert_eq!(
            bake.tribute.get(&c).map(|t| t.patron),
            Some(b),
            "precondition: B must hold C, or the guard under test is never reached"
        );
        // A could beat B (80 > 30 × RAID_MARGIN) and B is productive — the ONLY
        // thing protecting B is that it is already a patron.
        bake.maybe_raid(a, &era, 0.0);
        assert!(
            !bake.tribute.contains_key(&b),
            "a patron must not be subordinated: B holds C, so A must leave it"
        );
        assert_eq!(bake.tribute.len(), 1, "exactly one relation may stand");
        assert_no_chained_relations(&bake);
        assert_eq!(
            bake.tally.subordinations_formed, 1,
            "only B's bid may have formed anything"
        );
        assert_eq!(bake.tally.patronage_transfers, 0, "nothing changed hands");
    }

    /// A raider on prime land beside a subordinate on marginal land, plus the
    /// two indices. The value asymmetry is the point: `Subordinate` requires
    /// `t_val <= raider_val`, so a *poorer* target is legal, and it is the only
    /// shape that can tell "reads the subordinate's cell" from "reads the
    /// raider's" — which spec §4.2's information asymmetry turns on.
    #[test]
    fn the_assessment_reads_the_subordinates_cell_not_the_raiders() {
        let (geo, graphs, capacity, river_prox, refugia, era) =
            cascade_world(|c| if c == CellId(0) { RICH } else { POOR });
        let mut bake = hand_bake(&graphs, &capacity, &river_prox, &refugia, no_disposition());
        let far = geo.neighbors(CellId(0))[0];

        let raider = bake.open(
            KindId("goblin"),
            CellId(0),
            0.0,
            80.0,
            Founding::Genesis(CellId(0)),
            None,
            0.0,
        );
        // Population 5 on a cell worth 10: pressure 0.5, so it is productive
        // (`has_spoils`) and beatable, but its ground is worth a tenth of the
        // raider's — no eviction motive at all.
        let target = bake.open(
            KindId("kobold"),
            far,
            0.0,
            5.0,
            Founding::Genesis(far),
            None,
            0.0,
        );

        bake.maybe_raid(raider, &era, 0.0);

        let t = bake
            .tribute
            .get(&target)
            .copied()
            .expect("a relation formed");
        assert_eq!(t.patron, raider);
        assert_eq!(
            t.assessment.to_bits(),
            (POOR * ASSESS_RATE).to_bits(),
            "the demand must be assessed on the SUBORDINATE's land ({POOR}), not the patron's \
             ({RICH}): {}",
            t.assessment
        );
    }

    #[test]
    fn a_stronger_rival_takes_over_a_standing_relation() {
        // Spec §4.4: a second bid TRANSFERS the patronage; the incumbent does
        // not contest. Nothing bound this before, so a "don't re-subordinate"
        // tweak could have deleted the rule with every test still green.
        let (geo, graphs, capacity, river_prox, refugia, era) = cascade_world(|_| RICH);
        let mut bake = hand_bake(&graphs, &capacity, &river_prox, &refugia, no_disposition());
        let ring = geo.neighbors(CellId(0));
        let (incumbent_cell, rival_cell) = (ring[0], ring[1]);

        let target = bake.open(
            KindId("goblin"),
            CellId(0),
            0.0,
            10.0,
            Founding::Genesis(CellId(0)),
            None,
            0.0,
        );
        let incumbent = bake.open(
            KindId("kobold"),
            incumbent_cell,
            0.0,
            20.0,
            Founding::Genesis(incumbent_cell),
            None,
            0.0,
        );
        // 80 clears 20 × RAID_MARGIN comfortably: this bid qualifies.
        let rival = bake.open(
            KindId("bugbear"),
            rival_cell,
            0.0,
            80.0,
            Founding::Genesis(rival_cell),
            None,
            0.0,
        );

        bake.maybe_raid(incumbent, &era, 0.0);
        assert_eq!(
            bake.tribute.get(&target).map(|t| t.patron),
            Some(incumbent),
            "precondition: the incumbent must hold the target"
        );

        bake.maybe_raid(rival, &era, 0.0);
        assert_eq!(
            bake.tribute.get(&target).map(|t| t.patron),
            Some(rival),
            "a qualifying second bid must MOVE the patronage, not be ignored"
        );
        assert_eq!(bake.tribute.len(), 1, "a transfer adds no second relation");
        assert_eq!(
            bake.tally.subordinations_formed, 1,
            "one people was subjugated, once — a takeover subjugates nobody new"
        );
        assert_eq!(bake.tally.patronage_transfers, 1, "one takeover");
        assert_no_chained_relations(&bake);
    }

    #[test]
    fn a_rival_that_cannot_out_muscle_the_incumbent_leaves_the_relation_alone() {
        // Spec §4.4's hysteresis, revised on measurement: without it the rule
        // produced ~87% churn, rivals swapping the same targets every epoch.
        // The rival here plainly beats the SUBORDINATE (25 > 10 × RAID_MARGIN)
        // and fails only against the INCUMBENT (25 <= 20 × RAID_MARGIN), so the
        // margin's subject is the single thing this test reads.
        let (geo, graphs, capacity, river_prox, refugia, era) = cascade_world(|_| RICH);
        let mut bake = hand_bake(&graphs, &capacity, &river_prox, &refugia, no_disposition());
        let ring = geo.neighbors(CellId(0));
        let (incumbent_cell, rival_cell) = (ring[0], ring[1]);

        let target = bake.open(
            KindId("goblin"),
            CellId(0),
            0.0,
            10.0,
            Founding::Genesis(CellId(0)),
            None,
            0.0,
        );
        let incumbent = bake.open(
            KindId("kobold"),
            incumbent_cell,
            0.0,
            20.0,
            Founding::Genesis(incumbent_cell),
            None,
            0.0,
        );
        let rival = bake.open(
            KindId("bugbear"),
            rival_cell,
            0.0,
            25.0,
            Founding::Genesis(rival_cell),
            None,
            0.0,
        );

        bake.maybe_raid(incumbent, &era, 0.0);
        assert_eq!(
            bake.tribute.get(&target).map(|t| t.patron),
            Some(incumbent),
            "precondition: the incumbent must hold the target"
        );

        bake.maybe_raid(rival, &era, 0.0);
        assert_eq!(
            bake.tribute.get(&target).map(|t| t.patron),
            Some(incumbent),
            "a bid that cannot out-muscle the incumbent must leave the relation standing"
        );
        assert_eq!(
            bake.tally.patronage_transfers, 0,
            "no takeover may be counted"
        );
        assert_eq!(
            bake.tally.subordinations_formed, 1,
            "and nothing new was formed either"
        );
    }

    #[test]
    fn a_second_scan_on_a_target_already_ours_forms_nothing() {
        // The "already ours" skip: there is nothing further to take from one's
        // own subordinate this epoch. Without it the same relation would be
        // re-inserted every scan and counted as a takeover of itself.
        let (geo, graphs, capacity, river_prox, refugia, era) = cascade_world(|_| RICH);
        let mut bake = hand_bake(&graphs, &capacity, &river_prox, &refugia, no_disposition());
        let far = geo.neighbors(CellId(0))[0];

        let raider = bake.open(
            KindId("goblin"),
            CellId(0),
            0.0,
            80.0,
            Founding::Genesis(CellId(0)),
            None,
            0.0,
        );
        let target = bake.open(
            KindId("kobold"),
            far,
            0.0,
            10.0,
            Founding::Genesis(far),
            None,
            0.0,
        );

        bake.maybe_raid(raider, &era, 0.0);
        let after_first = (
            bake.tally.subordinations_formed,
            bake.tally.patronage_transfers,
        );
        assert_eq!(after_first, (1, 0), "precondition: one first-time relation");

        bake.maybe_raid(raider, &era, 0.0);
        assert_eq!(
            (
                bake.tally.subordinations_formed,
                bake.tally.patronage_transfers
            ),
            after_first,
            "re-scanning one's own subordinate must move no counter"
        );
        assert_eq!(
            bake.tribute.get(&target).map(|t| t.patron),
            Some(raider),
            "and must leave the relation exactly as it stood"
        );
    }

    #[test]
    fn a_tribute_relation_dies_with_either_party() {
        // Spec §4.4's coherence floor. `tribute` holds community INDICES, so
        // an entry naming a closed community is a dangling index — the kind of
        // corruption that stays silent until it panics on an unrelated seed.
        // Both roles must be cleaned: subordinate (the key) and patron (the
        // value). The freed subordinate does NOT cascade — collapse-release is
        // an explicit §9 non-goal.
        let (geo, graphs, capacity, river_prox, refugia, _era) = cascade_world(|_| RICH);
        let mut bake = hand_bake(&graphs, &capacity, &river_prox, &refugia, no_disposition());
        let ring = geo.neighbors(CellId(0));
        let (cell_a, cell_b) = (ring[0], ring[1]);

        let patron = bake.open(
            KindId("goblin"),
            CellId(0),
            0.0,
            80.0,
            Founding::Genesis(CellId(0)),
            None,
            0.0,
        );
        let sub_a = bake.open(
            KindId("kobold"),
            cell_a,
            0.0,
            10.0,
            Founding::Genesis(cell_a),
            None,
            0.0,
        );
        let sub_b = bake.open(
            KindId("bugbear"),
            cell_b,
            0.0,
            10.0,
            Founding::Genesis(cell_b),
            None,
            0.0,
        );
        for &s in &[sub_a, sub_b] {
            bake.tribute.insert(
                s,
                Tribute {
                    patron,
                    assessment: 1.0,
                    since: 0.0,
                },
            );
        }

        // (a) The SUBORDINATE falls: its own relation goes, its sibling's stays.
        bake.close(sub_a, 100.0, CauseOfEnd::Famine, Ended::Nature);
        assert!(
            !bake.tribute.contains_key(&sub_a),
            "a dead subordinate pays nobody"
        );
        assert_eq!(
            bake.tribute.get(&sub_b).map(|t| t.patron),
            Some(patron),
            "one subordinate's death must not dissolve its sibling's relation"
        );

        // (b) The PATRON falls: every relation it held goes with it, and the
        //     freed subordinate lives on where it stood (no cascade).
        bake.close(patron, 200.0, CauseOfEnd::Famine, Ended::Nature);
        assert!(
            bake.tribute.is_empty(),
            "a dead patron collects from nobody: {:?}",
            bake.tribute.keys().collect::<Vec<_>>()
        );
        assert!(
            bake.communities[sub_b].alive,
            "the freed subordinate must survive its patron"
        );
        assert_eq!(
            bake.node_index.get(&cell_b),
            Some(&sub_b),
            "the freed subordinate keeps its cell"
        );
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
        let (_geo, graphs, capacity, river_prox, refugia, era) =
            cascade_world(|c| if c == CellId(20) { RICH } else { POOR });
        let mut bake = hand_bake(&graphs, &capacity, &river_prox, &refugia, no_disposition());

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
        // …and the tally must SAY so (The Tumult, final review F-2). This is
        // the terminal hop of a cascade, not a top-level relocation; while
        // `resettled` was incremented at `maybe_raid`'s call site only, it went
        // uncounted and the field under-reported its own doc comment. Seed 42
        // cannot catch this — its single cascade's victim died on the road
        // rather than reaching vacant land — so this is the only arm the fix
        // has.
        assert_eq!(
            bake.tally.resettled, 1,
            "the evicted holder reached vacant land: that is a resettle, at whatever \
             depth of the cascade it happens"
        );
    }

    #[test]
    fn a_resettle_at_the_head_of_a_relaxation_is_counted_exactly_once() {
        // The other side of F-2's accounting: moving the `resettled` increment
        // down into `relocate` must not double-count the ordinary case, where
        // `maybe_raid`'s loser reaches vacant land in one hop and the top-level
        // call site used to be what tallied it. One displaced people, one
        // resettle — restoring the old call-site increment alongside the new
        // one makes this read 2.
        let probe = Geosphere::new(1);
        let target_cell = probe.neighbors(CellId(0))[0];
        let (_geo, graphs, capacity, river_prox, refugia, era) =
            cascade_world(|c| if c == target_cell { 110.0 } else { 100.0 });
        let mut bake = hand_bake(&graphs, &capacity, &river_prox, &refugia, no_disposition());
        // A strong raider on land worth less than its neighbour's, and a fed,
        // beatable target holding the better cell.
        let raider = bake.open(
            KindId("kobold"),
            CellId(0),
            0.0,
            200.0,
            Founding::Genesis(CellId(0)),
            None,
            0.0,
        );
        bake.open(
            KindId("goblin"),
            target_cell,
            0.0,
            50.0,
            Founding::Genesis(target_cell),
            None,
            0.0,
        );
        bake.maybe_raid(raider, &era, 0.0);

        assert_eq!(bake.tally.raided, 1, "the fixture must reach a raid");
        assert_eq!(
            bake.tally.resettled, 1,
            "the loser found vacant land in one hop: exactly one resettle"
        );
        assert_eq!(
            bake.tally.cascade_hist, [0u64; CASCADE_BINS],
            "nobody was displaced onward, so no cascade is recorded"
        );
    }

    #[test]
    fn a_people_that_cannot_survive_winning_does_not_fight() {
        // The Tumult, final review F-3: `relocate`'s `can_fight` guard shipped
        // with NO arm anywhere — replacing it with `true` left all 22 bake
        // tests, the end-to-end no-peopleless-settlements gate, and seed 42's
        // whole census byte-identical. Seed 42 simply never produces a roller
        // in the narrow band the guard governs, so only a hand-built state can
        // reach it.
        //
        // The band is exactly `[VIABLE_MIN, VIABLE_MIN / (1 - WAR_LOSS))` =
        // [2.0, 2.857): big enough to keep looking for a home (the `pop <
        // VIABLE_MIN` death above does not catch it), too small to still be
        // viable after paying `WAR_LOSS` for a conquest. Such a people must
        // pioneer, never prey — and a people just ABOVE the band must still
        // prey, or the guard would be a blanket ban on weak conquerors rather
        // than the viability rule it is. Both halves are asserted, so the test
        // pins where the threshold sits and not merely that one exists.
        let take_the_rich_cell = |roller_pop: f64| {
            let (_geo, graphs, capacity, river_prox, refugia, era) =
                cascade_world(|c| if c == CellId(20) { RICH } else { POOR });
            let mut bake = hand_bake(&graphs, &capacity, &river_prox, &refugia, no_disposition());

            // A very weak holder sits on the one rich cell: beatable by even a
            // sub-viable roller (`RAID_MARGIN` clears at strength 1.5+), and
            // far enough below its cell's capacity to have spoils worth taking.
            bake.open(
                KindId("goblin"),
                CellId(20),
                0.0,
                1.0,
                Founding::Genesis(CellId(20)),
                None,
                0.0,
            );
            let roller = bake.open(
                KindId("kobold"),
                CellId(0),
                0.0,
                roller_pop,
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
                roller_pop,
                r_lineage,
                r_id,
                0.0,
                CellId(0),
                &era,
                0.0,
                0,
            );
            let seated = *bake
                .node_index
                .get(&CellId(20))
                .expect("the rich cell is occupied either way");
            let holder_people = bake.records[bake.communities[seated].record].people;
            (outcome, holder_people)
        };

        // Inside the band: 2.5 clears `VIABLE_MIN` but 2.5 × 0.7 = 1.75 does
        // not, so held ground never enters the option set. It pioneers onto
        // marginal vacant land and the goblins keep the rich cell.
        let (outcome, holder) = take_the_rich_cell(2.5);
        assert_eq!(
            outcome,
            Relocation::Settled { cascade: 0 },
            "a roller that could not survive winning must pioneer, not prey"
        );
        assert_eq!(
            holder,
            KindId("goblin"),
            "the rich cell must not have changed hands: a sub-viable conqueror \
             would hold it as a remnant this model already calls dead"
        );

        // Just above it: 2.9 × 0.7 = 2.03 clears `VIABLE_MIN`, so the very same
        // world resolves the other way. Without this half the assertion above
        // would also pass if the guard vetoed every weak roller outright.
        let (outcome, holder) = take_the_rich_cell(2.9);
        assert_eq!(
            outcome,
            Relocation::Settled { cascade: 1 },
            "a roller that CAN survive winning still takes the rich held cell"
        );
        assert_eq!(
            holder,
            KindId("kobold"),
            "the rich cell must have changed hands just above the threshold"
        );
    }

    #[test]
    fn the_settled_premium_makes_a_held_cell_outbid_an_equal_vacant_one() {
        // The only term in the model that RAISES conflict (spec §4.1): a held
        // cell is worth more than an empty cell of equal capacity, because a
        // rival's holding comes already made to work. With the premium at 0
        // the roller takes the equally-rich EMPTY cell (no defender) and the
        // branching ratio collapses again.
        //
        // Cells 18 and 20 are BOTH direct neighbours of cell 0 — the same ring.
        // That is deliberate and is the whole point of spec §4.3's locality
        // clause: the premium decides between a vacant and a held cell *at the
        // same distance*, which is the only place it should decide. Put the
        // empty rich cell further out and distance, not the premium, would be
        // doing the work.
        let (_geo, graphs, capacity, river_prox, refugia, era) = cascade_world(|c| {
            if c == CellId(20) || c == CellId(18) {
                RICH
            } else {
                POOR
            }
        });
        let mut bake = hand_bake(&graphs, &capacity, &river_prox, &refugia, no_disposition());

        // Cell 20 is rich AND held by a beatable community; cell 18 is rich
        // and empty. Equal capacity — only the premium separates them. (The
        // fixture named cell 30 before the locality fix reshaped it onto the
        // same ring; the comment lagged. Final review F-6.)
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
    fn a_roller_takes_a_near_home_over_a_richer_distant_one() {
        // Spec §4.3's locality clause. The scan stops at the FIRST ring that
        // offers anything admissible, so a remnant never crosses a landmass
        // for a better cell: ten times the capacity, three hops away, loses to
        // marginal land next door. Against an unrestricted scan over the whole
        // connected component this test fails — the roller seats itself on
        // CellId(30) instead, and the occupied set of a real world drifts
        // toward the globe's high-capacity cells.
        let (geo, graphs, capacity, river_prox, refugia, era) =
            cascade_world(|c| if c == CellId(30) { RICH } else { POOR });
        let mut bake = hand_bake(&graphs, &capacity, &river_prox, &refugia, no_disposition());

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
            Relocation::Settled { cascade: 0 },
            "vacant land was available next door — no conflict was needed"
        );
        let seat = bake
            .communities
            .iter()
            .find(|c| c.alive)
            .expect("the roller must have settled somewhere")
            .site;
        assert!(
            geo.neighbors(CellId(0)).contains(&seat),
            "the roller must settle in the nearest ring, not cross the world for CellId(30): sat on {seat:?}"
        );
        // Within that ring every cell is equally poor and equally undefended,
        // so the CellId tie-break decides — a total, deterministic order.
        assert_eq!(seat, CellId(12), "the ring's tie-break must be total");
    }

    #[test]
    fn a_roller_widens_its_search_past_an_unusable_neighbourhood() {
        // The other half of spec §4.3's locality clause, and what separates it
        // from a naive one-ring scan: the search WIDENS. With the first two
        // rings turned uninhabitable there is nothing admissible near at all,
        // so the roller keeps walking outward and settles in the third ring —
        // a people whose whole neighbourhood is unusable still migrates as far
        // as it must. (Capacity is uniform, so nothing but distance orders the
        // options; an unrestricted scan takes the globally lowest `CellId`,
        // which sits a ring further out again.)
        let (geo, graphs, capacity, river_prox, refugia, era) = cascade_world(|_| POOR);
        let blocked: BTreeSet<CellId> = geo
            .cells()
            .filter(|&c| matches!(geo.hops_between(CellId(0), c, 16), Some(1 | 2)))
            .collect();
        let era = EraClimate {
            habitable: CellMap::from_fn(&geo, |c| !blocked.contains(&c)),
            ..era
        };
        let mut bake = hand_bake(&graphs, &capacity, &river_prox, &refugia, no_disposition());

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
        assert_eq!(outcome, Relocation::Settled { cascade: 0 });
        let seat = bake
            .communities
            .iter()
            .find(|c| c.alive)
            .expect("the roller must have settled somewhere")
            .site;
        assert_eq!(
            geo.hops_between(CellId(0), seat, 16),
            Some(3),
            "the roller must widen its search to the nearest usable ring, and stop there: sat on {seat:?}"
        );
    }

    #[test]
    fn a_weak_roller_flees_to_the_empties_instead_of_preying() {
        // The emergent half of spec §4.3: there is no `if migrating else
        // raiding` branch. A remnant too weak to clear the dominance margin
        // never sees the held cell in its option set at all, so it pioneers —
        // the same one rule, a different outcome.
        let (_geo, graphs, capacity, river_prox, refugia, era) =
            cascade_world(|c| if c == CellId(20) { RICH } else { POOR });
        let mut bake = hand_bake(&graphs, &capacity, &river_prox, &refugia, no_disposition());

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

    /// The three cells of [`a_cascade_runs_three_hops_and_dies_of_dissipation`]'s
    /// strength ladder, in the order the chain takes them. Each is a `Geosphere::new(1)`
    /// neighbour of the one before it, and all three are neighbours of `CellId(0)`
    /// (the origin the first roller is driven off), so every hop finds its
    /// next prize in ring 1 and the widening scan never has to look past it.
    const LADDER: [CellId; 3] = [CellId(20), CellId(12), CellId(14)];

    /// The people holding the bottom rung of
    /// [`a_cascade_runs_three_hops_and_dies_of_dissipation`]'s ladder — the one
    /// the chain spits out, whose survival the two arms flip.
    const TERMINAL: KindId = KindId("bugbear");

    #[test]
    fn a_cascade_runs_three_hops_and_dies_of_dissipation() {
        // The campaign's deliverable IS the cascade-size distribution, so the
        // multi-hop accumulation has to be exercised directly: the live worlds
        // measured so far fire only size-1 cascades, which leaves `1 +
        // victim_cascade` beyond one hop, the roller-side vetoes deeper in a
        // chain, and — the central physical claim of `Bake::relocate`'s doc —
        // that a chain ends by DISSIPATION rather than by `CASCADE_DEPTH_CAP`
        // all unverified. (The sibling depth-cap test reaches the cap only by
        // being handed `depth = CAP - 1`; that proves the guard, not the
        // physics.)
        //
        // The fixture is a descending strength ladder on `LADDER`: each holder
        // is beatable by the remnant the hop before it produced, and by nobody
        // weaker. Capacity picks the route (each rung is worth far more than
        // the POOR vacant land around it, so the held cell always outbids
        // pioneering within the ring), and population picks how far the chain
        // gets. Arithmetic, all at Neolithic weight 1.0 in year 0:
        //
        //   hop 1  roller 400 > 120 × RAID_MARGIN  → seats on 20; A carries
        //          120 × (1-WAR_LOSS) × MIGRATE_SURVIVAL = 75.6
        //   hop 2  A 75.6 > 20 × RAID_MARGIN       → seats on 12; B carries 12.6
        //   hop 3  B 12.6 > `weakest` × RAID_MARGIN → seats on 14; C carries
        //          `weakest` × 0.63
        //
        // `weakest` is the one knob, and it straddles the viable minimum:
        // 3.0 → the last remnant carries 1.89 < VIABLE_MIN and dies on the
        // road; 4.0 → it carries 2.52 and lives to settle. Everything else
        // about the two arms is identical, which is what makes the first arm's
        // termination attributable to dissipation and to nothing else — there
        // is vacant POOR land in ring 1 of every rung, so a remnant that had
        // anything left would always have had somewhere to go.
        let run = |weakest: f64| {
            let (_geo, graphs, capacity, river_prox, refugia, era) = cascade_world(|c| match c {
                // Descending prizes. Each is worth more than POOR × (1 +
                // SETTLED_PREMIUM), so a held rung always outbids the vacant
                // cells sharing its ring — and each exceeds its holder's
                // population, so no rung is a spoils-less husk.
                c if c == LADDER[0] => 200.0,
                c if c == LADDER[1] => 100.0,
                c if c == LADDER[2] => 50.0,
                _ => POOR,
            });
            let mut bake = hand_bake(&graphs, &capacity, &river_prox, &refugia, no_disposition());
            let holders = [
                (KindId("goblin"), 120.0),
                (KindId("hobgoblin"), 20.0),
                (TERMINAL, weakest),
            ];
            for (&cell, &(people, pop)) in LADDER.iter().zip(holders.iter()) {
                bake.open(people, cell, 0.0, pop, Founding::Genesis(cell), None, 0.0);
            }
            let roller = bake.open(
                KindId("kobold"),
                CellId(0),
                0.0,
                400.0,
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
                400.0,
                r_lineage,
                r_id,
                0.0,
                CellId(0),
                &era,
                0.0,
                0,
            );
            // Read everything the assertions need out of the borrowed `Bake`
            // before it dies with this closure's frame: the outcome, the
            // tallies, who ended up seated on each rung, and whether the
            // bottom of the ladder is still anywhere in the world.
            let seated = LADDER.map(|cell| {
                bake.node_index
                    .get(&cell)
                    .map(|&i| bake.records[bake.communities[i].record].people)
            });
            let terminal_survived = bake
                .communities
                .iter()
                .any(|c| c.alive && bake.records[c.record].people == TERMINAL);
            (
                outcome,
                (bake.tally.raided, bake.tally.fled, bake.tally.collapsed),
                seated,
                terminal_survived,
            )
        };

        // ---- Arm 1: the last remnant dissipates below VIABLE_MIN. ----------
        let (outcome, tally, seated, terminal_survived) = run(3.0);
        assert_eq!(
            outcome,
            Relocation::Settled { cascade: 3 },
            "the chain must accumulate one displacement per hop: three holders \
             evicted is `1 + (1 + (1 + 0))`, not a single hop and not a \
             truncated count"
        );
        let Relocation::Settled { cascade } = outcome else {
            unreachable!("asserted Settled above")
        };
        assert!(
            cascade < CASCADE_DEPTH_CAP,
            "the chain must end on its own, not at the safety bound: {cascade} \
             reached CASCADE_DEPTH_CAP ({CASCADE_DEPTH_CAP})"
        );
        assert_eq!(
            (tally.0, tally.1),
            (3, 3),
            "three hops, three evictions (raided, fled)"
        );
        // Each rung is now held by the people one rung UP from it — the
        // signature of a chain, not of three unrelated raids.
        assert_eq!(
            seated,
            [
                Some(KindId("kobold")),
                Some(KindId("goblin")),
                Some(KindId("hobgoblin")),
            ],
            "each rung must be held by the people the hop before it displaced"
        );
        // The bottom of the ladder is gone from the world, and counted.
        assert!(
            !terminal_survived,
            "the terminal remnant must be gone: it carried 1.89 < VIABLE_MIN"
        );
        assert_eq!(
            tally.2, 1,
            "the remnant that died on the road must be tallied, not dropped"
        );

        // ---- Arm 2: the SAME chain, with a terminal remnant just above ----
        // VIABLE_MIN. Only `weakest` differs, so the flipped outcome isolates
        // the viable-minimum floor as the first arm's cause of death.
        let (outcome, tally, _, terminal_survived) = run(4.0);
        assert_eq!(
            outcome,
            Relocation::Settled { cascade: 3 },
            "the chain is the same length either way — what changes is the fate \
             of the remnant it spits out"
        );
        assert!(
            terminal_survived,
            "carrying 2.52 ≥ VIABLE_MIN, the terminal remnant must survive to \
             settle — proving arm 1's remnant died of dissipation and not of \
             having nowhere to go"
        );
        assert_eq!(tally.2, 0, "nothing was lost on the road in this arm");
    }

    #[test]
    fn the_depth_cap_truncates_a_cascade_and_the_dropped_remnant_is_tallied() {
        // Two bounds in one fixture. (a) `CASCADE_DEPTH_CAP` is a hard stop:
        // at the cap nothing is opened at all. (b) One hop below it, the
        // displacement still happens and the victim's own relocation is
        // truncated — and that lost victim MUST be counted (a Task-1 review
        // defect: the recursion dropped it silently while the top-level call
        // mapped `Lost` to `collapsed`, so communities vanished uncounted).
        let (_geo, graphs, capacity, river_prox, refugia, era) =
            cascade_world(|c| if c == CellId(20) { RICH } else { POOR });
        let mut bake = hand_bake(&graphs, &capacity, &river_prox, &refugia, no_disposition());

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
    fn a_starving_target_is_not_worth_raiding() {
        // Inhibition 1 of spec §4.2a (momentary): a community already eating
        // everything its own land yields has no surplus to contend over, so it
        // is not a raid candidate however weak and however rich its ground.
        // Both arms are identical but for the target's population — the veto
        // is the ONLY thing that differs, and the fed arm proves the fixture
        // really does reach a raid.
        let probe = Geosphere::new(1);
        let target_cell = probe.neighbors(CellId(0))[0];
        let raid_with_target_pop = |target_pop: f64| {
            let (_geo, graphs, capacity, river_prox, refugia, era) =
                cascade_world(|c| if c == target_cell { 110.0 } else { 100.0 });
            let mut bake = hand_bake(&graphs, &capacity, &river_prox, &refugia, no_disposition());
            // The raider: strong, on land worth less than its neighbour's.
            let raider = bake.open(
                KindId("kobold"),
                CellId(0),
                0.0,
                200.0,
                Founding::Genesis(CellId(0)),
                None,
                0.0,
            );
            bake.open(
                KindId("goblin"),
                target_cell,
                0.0,
                target_pop,
                Founding::Genesis(target_cell),
                None,
                0.0,
            );
            bake.maybe_raid(raider, &era, 0.0);
            bake.tally.raided
        };

        // Fed (pressure 0.45): covetousness and dominance both hold, and the
        // raid fires — so nothing but the veto can explain the other arm.
        assert_eq!(
            raid_with_target_pop(50.0),
            1,
            "a target with a surplus must be raided"
        );
        // Starving (pressure 1.0, on capacity 110): same covetousness, same
        // dominance, nothing to take.
        assert_eq!(
            raid_with_target_pop(110.0),
            0,
            "a target already eating its whole yield has no spoils to take"
        );
    }

    #[test]
    fn a_roller_will_not_displace_a_starving_holder() {
        // The same veto, on the roll-downhill's side of the one rule — this is
        // the arm that blocks spec §4.2a's "pathological regress of remnants
        // preying on remnants all the way down".
        let roll_against_holder_pop = |holder_pop: f64| {
            let (_geo, graphs, capacity, river_prox, refugia, era) =
                cascade_world(|c| if c == CellId(20) { RICH } else { POOR });
            let mut bake = hand_bake(&graphs, &capacity, &river_prox, &refugia, no_disposition());
            bake.open(
                KindId("goblin"),
                CellId(20),
                0.0,
                holder_pop,
                Founding::Genesis(CellId(20)),
                None,
                0.0,
            );
            let roller = bake.open(
                KindId("kobold"),
                CellId(0),
                0.0,
                200.0,
                Founding::Genesis(CellId(0)),
                None,
                0.0,
            );
            let (r_id, r_lineage) = (
                bake.communities[roller].id,
                bake.communities[roller].lineage,
            );
            bake.close(roller, 0.0, CauseOfEnd::Fled, Ended::Nature);
            bake.relocate(
                KindId("kobold"),
                200.0,
                r_lineage,
                r_id,
                0.0,
                CellId(0),
                &era,
                0.0,
                0,
            )
        };

        // Fed holder (pressure 0.4 on RICH land): the roller takes it.
        assert_eq!(
            roll_against_holder_pop(40.0),
            Relocation::Settled { cascade: 1 },
            "a holder with a surplus is worth displacing"
        );
        // Starving holder (pressure 1.0 on the same RICH land): the roller
        // pioneers the marginal empties instead of taking a husk.
        assert_eq!(
            roll_against_holder_pop(RICH),
            Relocation::Settled { cascade: 0 },
            "a starving holder has nothing worth rolling over"
        );
    }

    #[test]
    fn a_timid_people_does_not_raid_however_strong_it_is() {
        // Inhibition 2 of spec §4.2a (durable): a people whose authored
        // `threat_response` falls below `RAID_DISPOSITION_MIN` never takes the
        // initiative, however strong it is on paper. Both arms are the same
        // world with the same populations — only the raider's disposition
        // differs — and the third arm pins the fail-open contract for a people
        // with no authored psyche at all.
        let probe = Geosphere::new(1);
        let target_cell = probe.neighbors(CellId(0))[0];
        let raid_with_disposition = |disposition: BTreeMap<KindId, f64>| {
            let (_geo, graphs, capacity, river_prox, refugia, era) =
                cascade_world(|c| if c == target_cell { 110.0 } else { 100.0 });
            let mut bake = hand_bake(&graphs, &capacity, &river_prox, &refugia, &disposition);
            let raider = bake.open(
                KindId("kobold"),
                CellId(0),
                0.0,
                200.0,
                Founding::Genesis(CellId(0)),
                None,
                0.0,
            );
            bake.open(
                KindId("goblin"),
                target_cell,
                0.0,
                50.0,
                Founding::Genesis(target_cell),
                None,
                0.0,
            );
            bake.maybe_raid(raider, &era, 0.0);
            bake.tally.raided
        };

        let bold: BTreeMap<KindId, f64> = [(KindId("kobold"), 0.9)].into_iter().collect();
        assert_eq!(
            raid_with_disposition(bold),
            1,
            "a people that stands its ground raids when motive and capability are there"
        );
        let timid: BTreeMap<KindId, f64> = [(KindId("kobold"), 0.2)].into_iter().collect();
        assert_eq!(
            raid_with_disposition(timid),
            0,
            "a people that flees does not take the initiative, however strong"
        );
        assert_eq!(
            raid_with_disposition(BTreeMap::new()),
            1,
            "a people with no authored disposition is not vetoed (fail-open)"
        );
    }

    #[test]
    fn a_timid_people_driven_off_its_land_flees_rather_than_rolling_over_a_holder() {
        // The same durable veto on the roll-downhill's side of the one rule: a
        // timid people driven off its land takes the marginal empties rather
        // than someone else's holding. Asymmetry falls out for free — a bold
        // neighbour would have rolled over the very same holder.
        let roll_with_disposition = |disposition: BTreeMap<KindId, f64>| {
            let (_geo, graphs, capacity, river_prox, refugia, era) =
                cascade_world(|c| if c == CellId(20) { RICH } else { POOR });
            let mut bake = hand_bake(&graphs, &capacity, &river_prox, &refugia, &disposition);
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
            bake.relocate(
                KindId("kobold"),
                50.0,
                r_lineage,
                r_id,
                0.0,
                CellId(0),
                &era,
                0.0,
                0,
            )
        };

        let bold: BTreeMap<KindId, f64> = [(KindId("kobold"), 0.9)].into_iter().collect();
        assert_eq!(
            roll_with_disposition(bold),
            Relocation::Settled { cascade: 1 },
            "a bold remnant rolls over the holder it can beat"
        );
        let timid: BTreeMap<KindId, f64> = [(KindId("kobold"), 0.2)].into_iter().collect();
        assert_eq!(
            roll_with_disposition(timid),
            Relocation::Settled { cascade: 0 },
            "a timid remnant pioneers instead — the same rule, a different people"
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
