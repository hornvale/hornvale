//! The deep-history bake: the forward simulation at the heart of the
//! living-community engine. It seeds an ancient world with a handful of
//! proto-communities, steps epochs across paleoclimate era-variance, and
//! resolves grow / found / migrate / raid / flee / collapse / resettle into
//! an occupation skeleton (a `Vec<BakeOccupation>` — alive and dead).
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
//! dynamics draw sequentially from one `history/bake/v2` stream in commit
//! order (bumped from `history/bake` by The Contour — decision 0006, an
//! epoch suffix, never a rename);
//! neighbour candidates sort by `f64::total_cmp`. Same seed ⇒ byte-identical
//! `records`.

use hornvale_history::record::{
    CauseOfEnd, Ended, Founding, Function, Notability, Occupation, TechHorizon,
};
use hornvale_kernel::seed::StreamLabel;
use hornvale_kernel::{CellId, CellMap, Geosphere, KindId, Seed, Stream};
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

/// The aggregate ease of reaching `cell`: the summed `conductance` of every
/// traversable edge into it (`conductance > 0.0` — ocean-touching adjacency
/// edges are stored at exactly 0.0 and are not routes). Higher means more,
/// and easier, ways in.
///
/// A pure function of the graph, with no time, seed, or bake state in it,
/// which is what makes [`defensibility`] recomputable and testable. The
/// graph is per-era, so this is too: a glacial low-stand that exposes a land
/// bridge raises the ease of every cell it reaches.
///
/// The `conductance > 0.0` filter is intent-documenting, not behaviourally
/// load-bearing for this sum: `conductance` is never negative by domain
/// convention (every producer in `graph_derive.rs` emits either `0.0` or a
/// positive reciprocal), so a filtered-out `0.0` term would contribute
/// nothing to the total even if the filter were removed. It stays because it
/// says explicitly, at the call site, "only traversable edges count" —
/// matching `traversable_neighbors`' idiom directly above — not because a
/// test can observe it changing this function's output.
///
/// Reserved integration seam: this campaign's Task 3 wires this into
/// `defensibility`, not yet present. Present in all builds (not
/// `#[cfg(test)]`-gated) so that seam is real, exercised here only by this
/// module's tests until it lands.
#[allow(dead_code)]
fn approach_ease(graph: &ConnectionGraph, cell: CellId) -> f64 {
    graph
        .edges(cell)
        .iter()
        .filter(|e| e.conductance > 0.0)
        .map(|e| e.conductance)
        .sum()
}

/// AUTHORED prior: the defensibility a wholly unobstructed approach tends to.
/// Approached, never attained — `tanh` is asymptotic in both directions.
/// Symmetric with `DEF_MAX` about 1.0, which is what lets the centred form
/// put the median approach at exactly 1.0 (spec §2.3).
/// type-audit: bare-ok(ratio: DEF_MIN)
const DEF_MIN: f64 = 0.75;
/// AUTHORED prior: the defensibility an infinitely dear approach tends to.
/// type-audit: bare-ok(ratio: DEF_MAX)
const DEF_MAX: f64 = 1.25;
/// CALIBRATED (Task 2d): the pooled median `cost_exponent` over 756,510
/// ordered pairs across seeds 1..=30, measured before any behavioural readout
/// existed and frozen thereafter (spec §4.4). Centring here is what makes the
/// MEDIAN approach in the world map to exactly 1.0. A save-format constant.
/// type-audit: bare-ok(ratio: DEF_CENTER)
const DEF_CENTER: f64 = 6.256709;
/// AUTHORED: how many log-cost units the transition spans. A SHAPE parameter,
/// not a scale of the quantity — the quantity's scale is `DEF_CENTER` — so
/// this is authored at 1.0 rather than fitted. At this value the land
/// population grades across 0.376, five times spec §4.4's trigger threshold.
/// type-audit: bare-ok(ratio: DEF_SCALE)
const DEF_SCALE: f64 = 1.0;

/// How well `to` is defended against an approach from `from`: a strictly
/// monotone, saturating function of the log traversal cost of the cheapest
/// route between them. A multiplier on the HOLDER's side of the dominance
/// test — the second contest axis (decision 0096 clause 1).
///
/// Reads the approach rather than the cell because the calibration found
/// approach ease is two disjoint regimes — water-connected and land-only —
/// which no single transform over an aggregate can grade (spec §2.3a). A raid
/// arrives along one route, and what shelters the defender is the resistance
/// of that route.
///
/// Parallel edges resolve by MAXIMUM conductance: an attacker takes the
/// easiest road, which is also why this cannot double-count the 6.7% of cells
/// carrying duplicate `to` values.
///
/// Pure in `(graph, from, to)` — no seed, no time, no bake state — so it
/// consumes no draw and cannot move stream consumption order. Returns
/// `DEF_MAX` for a nonexistent or wholly impassable link, which no caller
/// reaches: both call sites walk edges that exist.
fn defensibility(graph: &ConnectionGraph, from: CellId, to: CellId) -> f64 {
    let best = graph
        .edges(from)
        .iter()
        .filter(|e| e.to == to && e.conductance > 0.0)
        .map(|e| e.conductance)
        .fold(0.0_f64, f64::max);
    if best <= 0.0 {
        return DEF_MAX;
    }
    let cost_exponent = -hornvale_kernel::math::ln(best);
    let shaped = hornvale_kernel::math::tanh((cost_exponent - DEF_CENTER) / DEF_SCALE);
    DEF_MIN + (DEF_MAX - DEF_MIN) * (shaped + 1.0) / 2.0
}

/// Test-only re-export of [`defensibility`] so the property battery in
/// `tests/defensibility_field.rs` can reach it without making the field part
/// of this crate's real public surface.
/// type-audit: bare-ok(ratio: return)
#[doc(hidden)]
pub fn defensibility_for_test(graph: &ConnectionGraph, from: CellId, to: CellId) -> f64 {
    defensibility(graph, from, to)
}

/// The per-cell VIEW over [`defensibility`] (spec §2.4): the MINIMUM of
/// `defensibility(graph, from, cell)` over every `from` with a traversable
/// approach into `cell` — its weakest point, the quantity Ammann's envelope
/// model cares about. A place is only as defensible as its worst way in,
/// which is the same principle the mechanism itself applies from the other
/// end: `defensibility` resolves PARALLEL edges between the same pair of
/// cells by MAXIMUM conductance (an attacker always takes the easiest of
/// several roads to the SAME neighbour); this view takes the MINIMUM across
/// DISTINCT neighbours (a defender cannot choose which of several different
/// approaches an attacker picks). Two ends of one principle.
///
/// This is a view over the mechanism, not the mechanism: nothing in the bake
/// reads it (raiding still resolves per-edge, per spec §2.3a's finding that
/// no single aggregate grades both the water-connected and land-only
/// regimes), and it draws no seed and consumes no stream — pure in
/// `(graph, cell)`. Built for the almanac and for M4
/// (`defensibility-capacity-rank-corr`), per spec §2.4.
///
/// `DEF_MAX` — the same ceiling `defensibility` itself returns for a
/// nonexistent or wholly impassable link — for a cell with NO traversable
/// approach at all: an unreachable cell cannot be attacked, so it reads as
/// maximally (vacuously) defended rather than undefined.
///
/// Reads `cell`'s own edge list rather than scanning every node in the graph
/// for one pointing in: `ConnectionGraph::add_edge` mirrors every edge onto
/// both endpoints with the SAME conductance (its own doc comment), so this
/// graph is genuinely undirected, and `defensibility(graph, from, cell)` —
/// which internally reads `graph.edges(from)` — always agrees with the
/// matching entry already sitting in `graph.edges(cell)`. No reverse-
/// adjacency index is needed to enumerate "every `from` with an edge into
/// `cell`"; `traversable_neighbors(graph, cell)` already is that set.
/// type-audit: bare-ok(ratio: return)
pub fn weakest_point_defensibility(graph: &ConnectionGraph, cell: CellId) -> f64 {
    let approaches = traversable_neighbors(graph, cell);
    if approaches.is_empty() {
        return DEF_MAX;
    }
    approaches
        .into_iter()
        .map(|from| defensibility(graph, from, cell))
        .fold(f64::INFINITY, f64::min)
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
/// Drawn `threat_response` (flee 0 ↔ stand 1) at or above which a **community**
/// takes the initiative at all — spec §4.2a's durable inhibition.
///
/// **Since The Tolerance this gate sorts settlements, not peoples.** What it
/// compares is [`Community::disposition`]: a value drawn per settlement from
/// its people's authored `threat_response` (the mean) and `Dispersion::mind`
/// (the standard deviation), keyed on that settlement's own `(site, founded
/// year)`. Every earlier reading of this constant as a per-species flag — "this
/// people raids, that one does not" — is retired. No people is on one side of
/// it any more; each is a *distribution* straddling it, and every one of the
/// six settling peoples has settlements on both sides:
///
/// ```text
///   people      authored  σ(mind)  half-width  drawn range     ≈ share ABOVE 0.6
///   ----------  --------  -------  ----------  --------------  -----------------
///   gnoll           0.85     0.22       0.381  [0.47, 1.00]                  83 %
///   kobold          0.80     0.12       0.208  [0.59, 1.00]                  98 %
///   bugbear         0.80     0.20       0.346  [0.45, 1.00]                  79 %
///   hobgoblin       0.70     0.10       0.173  [0.53, 0.87]                  79 %
///   human           0.50     0.35       0.606  [0.00, 1.00]                  42 %
///   goblin          0.50     0.25       0.433  [0.07, 0.93]                  38 %
/// ```
///
/// (Half-width is √3·σ, the uniform draw's support; see
/// [`crate::disposition::people_disposition`]. Shares are of the *clamped*
/// support, and the clamp cannot move them: clamping to `[0, 1]` fixes every
/// value already inside it, so no draw crosses a threshold strictly inside
/// `(0, 1)` because of it.)
///
/// **Disclosure on the value, kept because it is still the reason 0.6 is 0.6.**
/// The gate's purpose was to make raiding heterogeneous; a threshold admitting
/// or vetoing the whole roster is inert by construction. 0.6 was chosen to sit
/// between the manikin's neutral midpoint — where goblin's authored temperament
/// sits (0.5) — and the assertive peoples (hobgoblin 0.7, kobold and bugbear
/// 0.8, gnoll 0.85), so exactly one of the five settling peoples declined to
/// raid on the pre-human roster, and (The Generalist) two of six once human was
/// authored at the same 0.5. That was a choice about what the gate *means*,
/// made against the authored roster and never fitted to a measured outcome; the
/// cascade metric was not consulted in picking it. Neither `threat_response`
/// nor this constant has moved since, including in The Tolerance — what moved
/// is that the roster's temperaments became distributions and the comparison
/// now happens per settlement.
///
/// A save-format constant: changing it re-fights every world's history.
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
///
/// **It is coupled to `GROWTH_RATE`, and the coupling is what decides whether
/// the demand binds at all.** A remittance is `min(assessment, surplus)`, and
/// the surplus is the logistic increment `GROWTH_RATE × N × (1 − N/eff)`,
/// maximised at `N = eff/2` and therefore never exceeding `GROWTH_RATE/4 ×
/// eff`. An `ASSESS_RATE` at or above `GROWTH_RATE/4` puts the assessment
/// beyond the largest surplus the land can ever yield, the surplus branch is
/// taken on every world, and the assessment — including anything §4.3's
/// adaptive loop does to it — is decorative. Half the ceiling
/// (`GROWTH_RATE/8`) is the value chosen: the demand then binds over the whole
/// middle band of the capacity curve (`N/eff ∈ (0.146, 0.854)`) rather than at
/// the single point `N = eff/2`, and there is headroom on both sides for the
/// adaptive loop to move it. Not fitted to any measured outcome; the coupling
/// itself is pinned by
/// `the_assessment_can_actually_bind_against_the_logistic_ceiling`.
const ASSESS_RATE: f64 = 0.025;
/// The ceiling on an assessment, as a multiple of the subordinate cell's
/// effective capacity: no patron may demand more than the land could ever
/// produce (spec §4.5's divergence bound). It does not bind at the moment a
/// relation forms — `ASSESS_RATE` is well under it — but it is the bound the
/// deferred adaptive-demand loop (§4.3) raises an assessment against, so the
/// clamp is written where the assessment is set rather than bolted on later.
const ASSESS_MAX: f64 = 0.5;
/// How fast a patron corrects its demand toward its subordinate's health — a
/// vassal that grew can bear more, one that shrank is being over-milked (spec
/// §4.3). The signal is two-signed, so this is a feedback loop rather than the
/// ratchet a shortfall-driven rule would give: `shortfall = assessment −
/// remittance` is non-negative by construction and can only ever push the
/// demand up.
///
/// A first-order feedback WITH DELAY (the delay is the epoch step)
/// period-doubles into chaos above a critical gain, so this constant carries a
/// stability claim — and spec §4.5 requires that claim be demonstrated rather
/// than asserted here. `a_long_run_assessment_neither_diverges_nor_absorbs_at_
/// zero` drives 200 epochs of a two-signed disturbance and holds the series to
/// a non-growing oscillation inside the clamp, which is the demonstration; this
/// comment is only its signpost. A save-format constant: changing it re-fights
/// every world's history.
const ADAPT_RATE: f64 = 0.2;
/// The population a vassal may be bled down TOWARD but never through (spec
/// §4.2b). A greedy patron shrinks its vassal — that is what closes the secular
/// cycle inside the mechanism, because a shrinking vassal is the only way the
/// health signal §4.3 feeds back on can go negative from tribute itself — but
/// tribute alone must never end a community, so the floor sits at or above
/// `VIABLE_MIN` and a bled vassal stays a viable community rather than a husk.
///
/// This constant is the amendment that reversed the earlier milk-don't-kill
/// cap. Under that cap a remittance could not exceed the epoch's growth
/// increment, so `population_after ≥ population_at_epoch_start` always and the
/// tribute loop's own signal was non-negative by construction: the demand eased
/// only when war, famine or climate hurt the vassal, never because the patron
/// over-extracted. A save-format constant: changing it re-fights every world's
/// history.
const FARM_FLOOR: f64 = VIABLE_MIN;
/// The most a maximally insular people withholds from its patron: the share of
/// its surplus a people with `in_group_radius == 0.0` keeps out of sight (spec
/// §4.2's concealment term). A maximally expansive people (`1.0`) conceals
/// nothing. A save-format constant: changing it re-fights every world's
/// history.
const CONCEAL_MAX: f64 = 0.5;
/// The horizon a patron whose people carries no authored `time_horizon` is
/// read at — the middle of the axis (spec §4.3a), never its bottom.
///
/// Every other authored-psychology lookup in this bake fails open to
/// "unaffected": an absent disposition does not veto ([`Bake::
/// takes_the_initiative`]), an absent `in_group_radius` conceals nothing
/// ([`Bake::concealment_of`]). There is no unaffected value on this axis —
/// the horizon does not switch extraction on or off, it only says *where* the
/// patron aims — and `0.0` is emphatically not it: zero is the shortest
/// sighted patron in the family, the one that strips its vassal to
/// `FARM_FLOOR` and holds it there. The neutral reading is therefore the
/// midpoint, which is also where two of the four authored settling peoples
/// (goblin, hobgoblin) already sit, so a bake handed no psyche data behaves
/// like a median patron rather than like the cruellest one.
const NEUTRAL_HORIZON: f64 = 0.5;
/// How many OTHER vassals a patron must hold for its effective horizon to fall
/// to half its authored one — spec §4.3c's portfolio effect, expressed as the
/// one number the shape needs.
///
/// A patron holding many vassals treats each as more expendable, so it extracts
/// harder from every one of them: the alternatives it has to any single
/// relation are what make that relation disposable. This is why empires are
/// crueller to distant provinces than to the core, and it produces the cruelty
/// **structurally** — from the shape of the relation table — rather than
/// authoring it as a personality.
///
/// **Why this matters beyond flavour.** The per-people axis
/// ([`Bake::horizon_of`]) is coarse and is partly confounded with `sociality`,
/// so a strategy read off `time_horizon` alone cannot be cleanly attributed in
/// any world-level reading. The holdings count is a property of the relation
/// table, so it varies independently of that confound.
///
/// The Tolerance (2026-08-04) re-derived that, because the numbers behind it
/// moved: this used to say the axis reaches only THREE values patron-side and
/// that the short extreme (bugbear, 0.3) is the only `Communal` short-horizon
/// people. Both halves are false, and the second one had ALREADY been false
/// before this campaign — The Vacancy's gnoll is authored at horizon 0.2 with
/// `threat_response` 0.85, so it has cleared the raid gate and outflanked
/// bugbear on the short end ever since, and it is `Hierarchic`, not `Communal`.
/// The Tolerance then falsified the first half too: `threat_response` is drawn
/// per settlement, so no people is vetoed out of patronhood and the whole
/// roster is reachable at **five distinct thresholds** ([`Bake::min_vassal`]).
/// The conclusion survives both: five cuts of one band is still coarse, and the
/// two `Communal` peoples remain bugbear (0.3) and kobold (0.8), so the horizon
/// axis still does not vary independently of social form across the roster.
///
/// Three is chosen against the measured range rather than fitted: live worlds
/// reach `max_subordinates` 6, so a patron at the observed maximum applies
/// `1/(1 + 5/3) = 0.375` of its authored horizon — a kobold at the top of the
/// distribution behaves about as an authored bugbear does (0.8 × 0.375 = 0.3,
/// bugbear's authored value exactly), which is the intended reading and not
/// further. Since The Tolerance made gnoll's 0.2 reachable patron-side, that is
/// the SECOND-shortest authored horizon rather than the shortest, so the
/// portfolio effect no longer quite reaches the short end of the axis — which
/// is the conservative direction, and is the same reading it always was: the
/// biggest holder becomes about as short-sighted as a genuinely short-sighted
/// people, never more so than any of them. A save-format constant: changing it
/// re-fights every world's history.
const PORTFOLIO_HALVING: f64 = 3.0;
/// The share of itself a vassal hands over in ONE epoch above which it stops
/// paying and **leaves** — spec §4.3d's flight, the first of the two answers
/// the subjugated have beyond concealment.
///
/// Derived from `GROWTH_RATE` rather than written down as a number, for the
/// same reason [`crash_basin_fraction`] is: it is not an independent parameter
/// and must never become one. The vassal's own per-capita increment is the
/// logistic `GROWTH_RATE × (1 − N/eff)`, whose supremum over every stock the
/// land admits is `GROWTH_RATE` itself. A remittance taking a larger share of
/// the community than that is therefore one **no management of its own stock
/// could ever regrow** — not merely a hard year, but a demand outside the
/// whole envelope of the vassal's biology. That is the natural reading of
/// "intolerable" in a model whose only growth law is this one, and it is the
/// strongest form of the statement the crash basin already makes from the
/// other side.
///
/// **Disclosure on the value, because a threshold is a choice about meaning.**
/// A weaker anchor was available and was considered: the road out costs
/// `1 − MIGRATE_SURVIVAL` (0.1) of the community, once, so a purely
/// arithmetical vassal would leave the moment a single epoch's tribute
/// exceeded the entire price of leaving — which, over an eighty-epoch bake,
/// is very nearly every standing relation. That reading makes flight the
/// *default* outcome of subordination rather than its limit, and would decide
/// spec §5's headline by evacuating the relation table instead of by letting
/// accumulated structure fail. `GROWTH_RATE` is chosen instead: the vassal
/// bears a burden it can grow back and leaves one it cannot. Twice the road
/// cost, and not fitted to any measured outcome — the cascade histogram was
/// not consulted in picking it. A save-format constant: changing it re-fights
/// every world's history.
const FLIGHT_BURDEN: f64 = GROWTH_RATE;

/// The lower edge of the band where a fresh relation can pay for itself, as a
/// fraction of the vassal's effective capacity — spec §4.3b's **low root**,
/// ≈ 0.1464 on the shipped constants.
///
/// A relation opens demanding `ASSESS_RATE × eff` from a vassal whose own
/// epoch increment is the logistic `GROWTH_RATE × N × (1 − N/eff)`. Writing
/// `x = N/eff`, the demand is payable out of growth exactly where
///
/// ```text
/// GROWTH_RATE × x × (1 − x)  ≥  ASSESS_RATE
/// ```
///
/// which is the interval between the two roots of that quadratic,
/// `x = (1 ∓ sqrt(1 − 4 × ASSESS_RATE / GROWTH_RATE)) / 2`. **Below the low
/// root the opening demand already exceeds everything the vassal can grow**,
/// so the patron is eating the stock from the first collection — the crash
/// basin. `ASSESS_RATE`'s own doc names this band from the other side (it is
/// why the rate sits at `GROWTH_RATE/8`); this function is the same algebra,
/// evaluated, so the two cannot drift apart. Derived from the constants rather
/// than written down as 0.1464 for exactly that reason — it is not an
/// independent parameter and must never become one.
///
/// Degenerate case, stated rather than assumed: at `ASSESS_RATE ≥
/// GROWTH_RATE/4` the discriminant is negative — the demand exceeds the
/// largest increment the land can ever yield, so *no* stock is farmable and
/// the two roots collapse onto `eff/2`. Clamping the discriminant at zero
/// returns exactly that (0.5), which is the truthful reading and not a
/// fallback. `the_assessment_can_actually_bind_against_the_logistic_ceiling`
/// pins the constants away from that regime.
///
/// Deterministic: `sqrt` is IEEE-exact and stays intrinsic (it is not one of
/// the libm-routed transcendentals), and the argument is a ratio of two
/// compile-time constants, so this is the same bit pattern on every platform.
fn crash_basin_fraction() -> f64 {
    let discriminant = (1.0 - 4.0 * ASSESS_RATE / GROWTH_RATE).max(0.0);
    (1.0 - discriminant.sqrt()) / 2.0
}

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

/// A handle to a community *inside the bake*, and nowhere else.
///
/// Deliberately not an `EntityId`: these live for the duration of one
/// simulation and are translated to real entities at emit. Deliberately not
/// `Serialize`/`Deserialize` either — a handle that never reaches the ledger
/// has no business being saveable, and `EntityId` deriving serde is exactly
/// what made the two easy to confuse.
/// type-audit: bare-ok(constructor-edge)
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct BakeId(pub u64);

/// One span of a people occupying a site, as the **bake** holds it. The
/// bake-side half of the pair.
///
/// Unlike [`OccupationRecord`] it knows its community and its lineage, because
/// the simulation tracks both; neither survives emit, because neither is
/// committed as a fact.
///
/// It deliberately does **not** delegate `is_alive`/`tenure` the way
/// [`OccupationRecord`] does. Bake-side callers write `.core.is_alive()`, and
/// the extra word is the point: inside the bake an occupation is one of
/// thousands being stepped, and the reminder that the liveness question is
/// asked of the shared core — not of the handle-bearing wrapper — is worth
/// more than the brevity.
#[derive(Clone, Debug, PartialEq)]
pub struct BakeOccupation {
    /// The facts both sides agree on.
    pub core: Occupation,
    /// The community this occupation belongs to.
    pub community: BakeId,
    /// The lineage this occupation continues.
    pub lineage: BakeId,
    /// How the occupation began.
    pub founded_from: Founding<BakeId>,
    /// How the occupation ended.
    pub ended_by: Ended<BakeId>,
}

/// Configuration for a deep-history bake: the span of years to simulate, the
/// epoch step, and the authored per-people psychology the raid and tribute
/// rules read (the raid rule's durable inhibition, and the subordinate's
/// concealment). Years are bare `f64` (absolute, no wall-clock). Not `Copy`:
/// the authored maps are owned, and the config is always passed by reference.
/// type-audit: bare-ok(count: start_year), bare-ok(count: end_year), bare-ok(count: epoch_years), bare-ok(ratio: disposition), bare-ok(ratio: disposition_spread), bare-ok(ratio: in_group_radius), bare-ok(ratio: time_horizon)
#[derive(Clone, Debug, PartialEq)]
pub struct BakeConfig {
    /// The year the ancient world is seeded at (inclusive).
    pub start_year: f64,
    /// The year the bake closes at (`now`); alive records keep `ended = None`.
    pub end_year: f64,
    /// The step between epochs, in years.
    pub epoch_years: f64,
    /// Each people's authored `threat_response` (flee 0 ↔ stand 1) — species
    /// data, looked up by the composition root and handed in here because the
    /// bake reads only kernel types.
    ///
    /// **Since The Tolerance this is the MEAN of a distribution, not the value
    /// the gate reads.** Each community draws its own `threat_response` out of
    /// `(this location, the matching `disposition_spread` entry)` at
    /// [`Bake::open`], keyed on its own `(site, founded year)`
    /// ([`crate::disposition::drawn_threat_response`]); the drawn value is what
    /// [`Bake::takes_the_initiative`] compares against `RAID_DISPOSITION_MIN`.
    /// A people ABSENT from this map is not vetoed at all — it draws nothing
    /// and passes the gate unconditionally, so a bake given no disposition data
    /// behaves exactly as it did before the gate existed.
    pub disposition: BTreeMap<KindId, f64>,
    /// Each people's `Dispersion::mind` — the standard deviation of the
    /// distribution whose mean is the matching `disposition` entry (spec D2),
    /// resolved by the composition root off `hornvale_species::
    /// dispersion_registry()` and handed in as a bare ratio for the same
    /// kernel-types-only reason.
    ///
    /// **A people absent here draws with spread 0**, i.e. exactly its authored
    /// location — the model's behaviour before The Tolerance. That is the
    /// fail-closed direction on purpose: a roster this registry does not cover
    /// (Lab's synthetic solo kinds re-key goblin's components under a fresh
    /// `KindId`) keeps a uniform people rather than acquiring a silent,
    /// unauthored spread.
    pub disposition_spread: BTreeMap<KindId, f64>,
    /// Each people's `SocietyVector.in_group_radius` (insular 0 ↔ expansive 1)
    /// — authored species data, never drawn, looked up by the composition root
    /// and handed in here because the bake reads only kernel types. It sets
    /// how much of its surplus a subordinate hides from its patron (spec
    /// §4.2's concealment term, `Bake::concealment_of`); a people ABSENT from
    /// the map conceals nothing, so a bake given no society data behaves
    /// exactly as it did before concealment existed.
    pub in_group_radius: BTreeMap<KindId, f64>,
    /// Each people's `MindVector.time_horizon` (immediate 0 ↔ generational 1)
    /// — authored species data, never drawn, looked up by the composition root
    /// and handed in here because the bake reads only kernel types. It is the
    /// **patron's** half of the negotiation: the discount rate a dominant
    /// applies to its vassal's future, which fixes the stock it steers that
    /// vassal toward (spec §4.3a, [`Bake::target_stock`]). A people ABSENT
    /// from the map is read at `NEUTRAL_HORIZON` — the middle of the authored
    /// axis, NOT zero, which would mean "strip to the floor" and is the
    /// harshest patron in the family rather than an unaffected one.
    ///
    /// **All six peoples are reachable patron-side, and that changed in The
    /// Tolerance.** The six settling peoples are authored gnoll 0.2 / bugbear
    /// 0.3 / goblin 0.5 / hobgoblin 0.5 / human 0.75 / kobold 0.8. Until this
    /// campaign, goblin's and human's `threat_response` (both authored 0.5)
    /// sat under `RAID_DISPOSITION_MIN` *as constants*, so no goblin or human
    /// community ever took the initiative, never became a patron, and their
    /// horizons were unreachable — leaving four peoples, three of them away
    /// from the neutral middle.
    ///
    /// That arithmetic is gone. `threat_response` is now drawn per settlement
    /// (`disposition`/`disposition_spread` above), so a *particular* goblin or
    /// human community can land above 0.6 and clear the gate. goblin's authored
    /// σ(mind) is 0.25 (half-width √3·σ = 0.433, so its draw spans ≈[0.07, 0.93]
    /// and about 38% of it clears 0.6) and human's is 0.35 (half-width 0.606,
    /// spanning the whole clamped axis, about 42% of it above the gate): both
    /// peoples reach patronhood on some settlements and not others.
    ///
    /// **Counted three ways, because two of them are easy to conflate**
    /// (`NEUTRAL_HORIZON` is 0.5):
    ///
    /// - **six PEOPLES** can now hold a vassal, up from four;
    /// - they take **five DISTINCT VALUES** — `{0.2, 0.3, 0.5, 0.75, 0.8}` —
    ///   because goblin and hobgoblin are both authored at 0.5;
    /// - **four of those peoples sit AWAY FROM the neutral middle** (gnoll,
    ///   bugbear, human, kobold), on the four values `{0.2, 0.3, 0.75, 0.8}`;
    ///   goblin and hobgoblin sit exactly *at* it and add no variety at all.
    ///
    /// So the variety this rule can produce is bounded by four horizons that
    /// differ from the middle, not six — and, since the gate is now a draw, by
    /// which *settlements* rather than which *peoples* happened to draw boldly.
    /// ([`Bake::min_vassal`] counts the same roster in a fourth way — five
    /// distinct *thresholds* — because it maps these values through a scale
    /// factor; both counts are stated there in full so neither has to be
    /// re-derived from this one.)
    pub time_horizon: BTreeMap<KindId, f64>,
}

impl BakeConfig {
    /// The default bake span: two millennia in 25-year epochs, with no
    /// authored psychology (nobody vetoed, nobody conceals, every patron reads
    /// at the neutral horizon — the composition root fills all three maps in).
    pub fn default_millennia() -> BakeConfig {
        BakeConfig {
            start_year: 0.0,
            end_year: 2000.0,
            epoch_years: 25.0,
            disposition: BTreeMap::new(),
            disposition_spread: BTreeMap::new(),
            in_group_radius: BTreeMap::new(),
            time_horizon: BTreeMap::new(),
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
    pub records: Vec<BakeOccupation>,
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
/// `community` field of a [`BakeOccupation`]), not by a bake-internal index,
/// so this survives the bake it came from — the same translation `Ended::By`
/// and `Founding::From` already rely on.
/// type-audit: bare-ok(count: since)
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct TributeRelation {
    /// The community that pays.
    pub subordinate: BakeId,
    /// The community that collects.
    pub patron: BakeId,
    /// The standard day this relation was established: when the *current*
    /// patron took it over, not when the subordinate first began paying
    /// somebody. A patronage transfer re-establishes it, and so does a
    /// relocation that re-seats the patron — the reseated lord is a new
    /// community, and this day is never earlier than the founding of either
    /// entity the emitted fact names.
    pub since: f64,
}

/// A tally of the events a bake resolved — the falsification instrument. Under
/// The Tumult's predation model the load-bearing counts are `raided`/`fled`
/// (conflict must fire on a *value* gradient, in worlds with land to spare, and
/// stay at zero in value-flat ones) read against `alive_at_now` (conquest must
/// redistribute the world, not depopulate it).
/// type-audit: bare-ok(count: grew), bare-ok(count: founded), bare-ok(count: migrated), bare-ok(count: raided), bare-ok(count: fled), bare-ok(count: collapsed), bare-ok(count: resettled), bare-ok(count: subordinations_formed), bare-ok(count: patronage_transfers), bare-ok(count: tribute_relations_at_now), bare-ok(count: max_subordinates), bare-ok(count: tribute_collected), bare-ok(count: max_stores_at_now), bare-ok(count: records_total), bare-ok(count: alive_at_now), bare-ok(count: cascade_hist), bare-ok(count: tribute_collection_events), bare-ok(count: vassal_flights), bare-ok(count: vassal_revolts)
// `Eq` is deliberately absent: the two accumulator readouts below are `f64`,
// and a census is only ever compared for equality in assertions (`PartialEq`),
// never used as a key.
#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct BakeCensus {
    /// Grow events (a community expanded under a sub-capacity load).
    pub grew: u64,
    /// Daughter-founding events (a comfortable community spawned a daughter).
    pub founded: u64,
    /// Migration events — an ORDERLY, self-directed move. Two paths reach it:
    /// a community relocated off a cell the era turned hostile, and (spec
    /// §4.3d) a vassal that walked away from a patron whose demand it would
    /// not go on paying. Both are leavings, so both belong here and neither
    /// belongs in `fled`, which means *driven off by a raider* and nothing
    /// else. `vassal_flights` below counts the second path on its own, so the
    /// two are separable without conflating either with eviction.
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
    /// The number of collection EVENTS resolved over the whole bake — one
    /// per (relation × epoch) pass through [`Bake::collect_tribute`]'s loop
    /// body for a still-live relation, incremented unconditionally there
    /// (even when the remittance it produces is `0.0`). A **flow**, exactly
    /// like `tribute_collected` above (which it is the event-count twin
    /// of), not a snapshot like `tribute_relations_at_now`.
    ///
    /// Added so rate and volume can be separated at all (T4 review, Important
    /// 2): `tribute_collected` alone confounds *how much per collection* with
    /// *how many collections happened*, and on seed 42 both moved at once —
    /// concealment lowered the per-collection rate but lengthened relations'
    /// standing lifespans, so the run total (a rate integrated over volume)
    /// moved the OPPOSITE way from the rate. `tribute_collected /
    /// tribute_collection_events` is the mean per-collection remittance; read
    /// alongside `tribute_relations_at_now` and `subordinations_formed` it is
    /// what an attribution needs to tell "collected less per visit" apart
    /// from "was visited more/fewer times".
    pub tribute_collection_events: u64,
    /// **Flights**: a vassal whose burden crossed `FLIGHT_BURDEN` closed its
    /// occupation and relocated rather than go on paying (spec §4.3d). A
    /// **flow**, and a strict subset of `migrated` — every flight is also an
    /// orderly move, and it is deliberately NOT in `fled`, which counts being
    /// driven off by a raider.
    ///
    /// **This counts departures, never deaths**, and the subset claim is
    /// enforced rather than argued: [`Bake::take_flight`] increments it on the
    /// `Settled` branches only. Two ways a flight fails to become a departure,
    /// and neither lands here — a vassal too small to survive the road never
    /// takes it, and one that takes it and finds nothing admissible anywhere is
    /// `collapsed`.
    pub vassal_flights: u64,
    /// **Revolts**: a standing relation dissolved because the vassal had come
    /// to out-muscle its patron by `RAID_MARGIN` (spec §4.3d). A **flow**, and
    /// the only path by which a relation ends with BOTH parties still alive
    /// and in place — dissolution-on-closure (spec §4.4) kills a community,
    /// and a patronage transfer replaces one patron with another rather than
    /// freeing anybody. Exactly one relation ends per event: the freed
    /// vassal's siblings are untouched, and any further loss is the patron's
    /// own weakening working through the shipped rules.
    pub vassal_revolts: u64,
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
    pub fn new(records: Vec<BakeOccupation>, now: f64) -> History {
        History {
            records,
            now,
            tribute: Vec::new(),
            tally: BakeCensus::default(),
        }
    }
}

/// One alive (or lately-dead) community's live state during the bake. The
/// `record` index ties it to its `BakeOccupation`; population is carried in
/// full `f64` precision.
struct Community {
    /// Index into `Bake::records` of this community's occupation record.
    record: usize,
    /// The cell this community currently occupies.
    site: CellId,
    /// The community's own entity handle.
    id: BakeId,
    /// The lineage this community continues (inherited by daughters/refounds).
    lineage: BakeId,
    /// Current population (full precision).
    population: f64,
    /// Whether the community is still alive.
    alive: bool,
    /// The tech horizon reached (monotone).
    tech: TechHorizon,
    /// Per-people tech-advance offset (years), drawn at genesis.
    tech_offset: f64,
    /// **This community's own drawn `threat_response`** — the raid gate's
    /// input (The Tolerance). Drawn once at [`Bake::open`] from its people's
    /// authored location and spread, keyed on this record's own `(site,
    /// founded year)`, and never recomputed: a record's site and founding year
    /// are immutable once opened, so caching the draw here is the same value
    /// [`crate::disposition::settlement_disposition`] will report off the
    /// committed ledger.
    ///
    /// `None` means the people carries no authored disposition at all and is
    /// not vetoed — the fail-open the gate has always had, preserved through
    /// the draw rather than around it.
    disposition: Option<f64>,
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
    /// [`Bake::collect_tribute`], which pays out the lesser of this and what
    /// the subordinate holds above `FARM_FLOOR` (spec §4.2b): the demand is
    /// what the patron *asks*, never what it necessarily gets.
    ///
    /// The term is *opened* the moment a relation forms because that is when
    /// the patron takes its reading of the land, not when it first comes to
    /// collect. Thereafter it is corrected at every collection against the
    /// subordinate's health (spec §4.3), so it is what this patron has learned
    /// about this vassal, not a number frozen at conquest.
    assessment: f64,
    /// The standard day this relation was established — the day the *current*
    /// patron took it. **Two things re-establish it**: a patronage transfer
    /// (some rival out-muscled the incumbent) and a relocation that re-seats
    /// the patron ([`Bake::carry_portfolio_to`]), because the reseated lord is
    /// a new community with a new [`BakeId`] and a fact naming it may not
    /// predate it. Carried only so the emitted fact can be dated by when it
    /// became true, exactly as an occupation's end-of-life facts are; no rule
    /// reads it.
    since: f64,
    /// The subordinate's population when this patron last collected — set at
    /// formation, updated at every collection. The health signal is measured
    /// against it, so the patron reacts to how its vassal has FARED rather
    /// than to a shortfall it can never see the far side of (spec §4.3: a
    /// shortfall is `assessment − remittance`, non-negative by construction,
    /// and so a ratchet).
    last_seen_population: f64,
}

/// The **portfolio** a relocating community takes with it: the relations it
/// held *as patron* at the instant it closed, lifted off the live table so
/// that they can be re-keyed onto the seat it reopens at (spec §4.3e).
///
/// This is the whole of the wounded-patron mechanism's state. It exists
/// because closing and reopening is one *movement* in this bake, not a death
/// and a birth: `Bake::close` dissolves both directions of every relation its
/// community was party to (spec §4.4's coherence floor), which is right for a
/// death and wrong for a move. Lifting first leaves `close` nothing to
/// dissolve, so **only a lift preserves** — a community that genuinely dies
/// (Famine, a remnant lost on the road, annihilation) never has one taken and
/// loses everything exactly as before.
///
/// **The continuity is role-asymmetric, and the asymmetry is the point**
/// (spec §4.3e). A relocating community keeps its relations as *patron* — a
/// lord's claim travels with him — and **drops them as subordinate**: a vassal
/// that flees is gone. So only the patron role is carried here; the
/// obligation is deliberately left in the table for `close` to dissolve.
/// Shipped role-blind first and measured: obligation that follows the runaway
/// turns flight into a change of address rather than an escape, and produced
/// repeat leavers (pooled flights 78 → 647, mostly the same communities
/// fleeing again every epoch because fleeing discharged nothing). Spec §4.3d's
/// flight is "relocates *rather than continue in that condition*", which only
/// means anything if leaving ends the condition.
#[derive(Clone, Debug, Default)]
struct CarriedPortfolio {
    /// The relations in which the carrier is the **patron**: its vassals, as
    /// `(subordinate index, terms)` pairs in ascending subordinate order
    /// (`BTreeMap` key order, so the carry is order-free).
    vassals: Vec<(usize, Tribute)>,
}

impl CarriedPortfolio {
    /// Nothing carried: the community held no vassals, or the caller is one
    /// for which continuity does not apply (a test fixture relocating a people
    /// that never held anything).
    ///
    /// Test-only: every production caller of [`Bake::relocate`] carries a real
    /// lift, because every production relocation is a community that closed
    /// somewhere and might have held vassals.
    #[cfg(test)]
    fn none() -> CarriedPortfolio {
        CarriedPortfolio::default()
    }
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
    /// The world's seed, kept so [`Bake::open`] can derive each community's
    /// own disposition draw. It is NOT another dynamics stream: the draw hangs
    /// off `settlement/disposition/v1`, keyed on the community's own `(site,
    /// founded year)`, and never touches `Bake::stream`.
    seed: Seed,
    /// Each people's authored `threat_response`, borrowed off the
    /// [`BakeConfig`] — the MEAN [`Bake::open`] draws each community's own
    /// disposition around, not the value the gate reads.
    disposition: &'a BTreeMap<KindId, f64>,
    /// Each people's `Dispersion::mind`, borrowed off the [`BakeConfig`] — the
    /// standard deviation of the draw above. Absent ⇒ spread 0 ⇒ the authored
    /// location exactly (the pre-Tolerance behaviour).
    disposition_spread: &'a BTreeMap<KindId, f64>,
    /// Each people's authored `in_group_radius`, borrowed off the
    /// [`BakeConfig`] — the concealment [`Bake::concealment_of`] reads.
    in_group_radius: &'a BTreeMap<KindId, f64>,
    /// Each people's authored `time_horizon`, borrowed off the [`BakeConfig`]
    /// — the patron's discount rate [`Bake::target_stock`] reads.
    time_horizon: &'a BTreeMap<KindId, f64>,
    /// Every occupation record, in commit order.
    records: Vec<BakeOccupation>,
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
    /// paid out of the epoch's growth first and only then out of the standing
    /// stock, down to `FARM_FLOOR` (spec §4.2b), so collection needs the
    /// increment `grow` actually applied — both to know what the surplus was
    /// and to tell it apart from the stock it stands on.
    ///
    /// Zeroed wholesale at the top of every epoch by [`Bake::begin_epoch`]. A
    /// stale increment surviving into the next epoch would be taxed again, and
    /// again, on a community that never grew after it — the standing stock by
    /// another name, arrived at by accident.
    ///
    /// **The clear was load-bearing until amendment 4, and is not any more —
    /// stated because a stale claim here would be worse than none.** `grow`
    /// *accumulates* into this buffer rather than overwriting it, so an epoch
    /// that did not start from zero carries the previous epoch's surplus
    /// forward; under the pre-setpoint rule that surplus was taxed a second
    /// time, and `last_epochs_growth_is_never_taxed_twice` reddened when the
    /// clear was suppressed. Under spec §4.3a's setpoint the take is
    /// `population − target` however it is decomposed — the two terms
    /// [`Bake::collect_tribute`] splits it into shift in opposite directions
    /// by exactly the same amount when this buffer is stale — so a stale
    /// increment can no longer move a remittance at all. **Mutation-verified
    /// in that direction (T5c): suppressing the clear now reddens nothing.**
    ///
    /// The buffer is therefore read only for the harvest/bleed *split* — which
    /// part of the take came out of the year's increment and which out of the
    /// standing stock. That split is presentational today; it is kept because
    /// the deferred levers (spec §9's assessment staleness, and any rule that
    /// bounds a demand by production rather than by stock) need the increment
    /// back, and because losing it would leave `collect_tribute` unable to say
    /// what it is taking. Do not restate it as a bound.
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
    /// Mint a fresh, never-reused bake-local handle.
    fn mint(&mut self) -> BakeId {
        let id = BakeId(self.next_id);
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
        era: &EraClimate,
        from: CellId,
        strength: f64,
        can_fight: bool,
        disposition: Option<f64>,
    ) -> Option<HomeOption> {
        // Whether held ground is in this band's option set AT ALL — a
        // strictly narrower thing than the caller's `can_fight` ("would
        // survive winning"), so it gets its own name rather than shadowing the
        // parameter. The durable inhibition is a property of the band's own
        // drawn mind, not of any one candidate: a timid remnant simply never
        // sees held ground. (Before The Tolerance this took the `KindId` and
        // read a species constant, which is exactly what made warlikeness a
        // property of a kind.)
        let may_take_held_land = can_fight && self.takes_the_initiative(disposition);
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
                        if !may_take_held_land
                            || strength <= hs * defensibility(self.cur(), from, n) * RAID_MARGIN
                        {
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
    ///
    /// `carried` is the portfolio the relocating community held when it closed
    /// ([`Bake::lift_portfolio`], taken by the caller immediately before its
    /// `close`). It is re-keyed onto whichever seat this relocation reaches,
    /// and **dropped on every `Lost` branch** — a remnant that dies on the road
    /// dies with its vassals, exactly as spec §4.4 requires of any real death.
    /// A relocating community's own *obligation* never rides along: spec
    /// §4.3e's continuity is role-asymmetric, so the leaver's patron is left
    /// behind at the `close`. Callers with nothing to carry pass
    /// [`CarriedPortfolio::none`].
    #[allow(clippy::too_many_arguments)]
    fn relocate(
        &mut self,
        people: KindId,
        pop: f64,
        lineage: BakeId,
        predecessor: BakeId,
        offset: f64,
        from: CellId,
        era: &EraClimate,
        year: f64,
        depth: u32,
        carried: CarriedPortfolio,
        disposition: Option<f64>,
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
            era,
            from,
            roller_strength(pop, offset, year),
            can_fight,
            disposition,
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
            // It reopened, so it was a move and not a death: the vassals it
            // held come with it (spec §4.3e).
            self.carry_portfolio_to(new_idx, carried, year);
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
        // Read BEFORE the victim closes, `disposition` included: the remnant
        // carries the mind of the settlement it is about to lose down the
        // cascade, because a band on the road has no seat of its own to draw
        // one from.
        let (v_people, v_pop, v_lineage, v_offset, v_id, v_disposition) = {
            let c = &self.communities[victim];
            (
                self.records[c.record].core.people,
                c.population,
                c.lineage,
                c.tech_offset,
                c.id,
                c.disposition,
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
        // The roller keeps the vassals it holds; the victim keeps the vassals
        // IT holds and loses its own lord, because being driven off is a
        // relocation and a relocating community drops its obligation (spec
        // §4.3e's asymmetry). So a roller that conquers its own vassal's cell
        // loses that vassal: the entry re-keyed here is dissolved two lines
        // later by the victim's `close`, exactly as it would be for a vassal
        // that fled of its own accord. The order of these two lines no longer
        // decides anything — the guards read liveness, and every entry either
        // survives both orderings or neither — and is kept for readability.
        self.carry_portfolio_to(new_idx, carried, year);
        let victim_carried = self.lift_portfolio(victim);
        self.close(victim, year, CauseOfEnd::Fled, Ended::By(displacer_id));
        self.touch(new_idx, year);
        self.tally.raided += 1;
        self.tally.fled += 1;
        // The evicted occupant cascades onward, founded from its own (the
        // victim's) community id, carrying what the war and the road left it —
        // and the vassals it holds, since being driven off is a move and not a
        // death (spec §4.3e).
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
            victim_carried,
            v_disposition,
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

    /// [`Bake::relocate`] for a people that is party to no tribute relation —
    /// the shape every conflict fixture in this file's test module wants, and
    /// nothing more than the full call with [`CarriedPortfolio::none`]. It exists
    /// so those fixtures say *carrying nothing* explicitly instead of trailing
    /// an opaque argument, and so the relation-continuity tests, which do pass
    /// a carry, stand out as the ones exercising spec §4.3e.
    #[cfg(test)]
    #[allow(clippy::too_many_arguments)]
    fn relocate_holding_nothing(
        &mut self,
        people: KindId,
        pop: f64,
        lineage: BakeId,
        predecessor: BakeId,
        offset: f64,
        from: CellId,
        era: &EraClimate,
        year: f64,
        depth: u32,
        disposition: Option<f64>,
    ) -> Relocation {
        self.relocate(
            people,
            pop,
            lineage,
            predecessor,
            offset,
            from,
            era,
            year,
            depth,
            CarriedPortfolio::none(),
            disposition,
        )
    }

    /// A community's own drawn disposition, resolved at [`Bake::open`] — spec
    /// D5's middle term, and the whole of The Tolerance's behavioural change.
    ///
    /// `None` when the people carries no authored `threat_response` at all: the
    /// fail-open is on the *people*, decided before any draw, so a bake handed
    /// no disposition data draws nothing and behaves exactly as it did before
    /// the gate existed. A people that IS authored always draws — with spread 0
    /// if `disposition_spread` has no row for it, which returns its authored
    /// location unchanged.
    fn drawn_disposition(&self, people: KindId, site: CellId, founded: f64) -> Option<f64> {
        let location = *self.disposition.get(&people)?;
        let spread = self.disposition_spread.get(&people).copied().unwrap_or(0.0);
        Some(crate::disposition::drawn_threat_response(
            self.seed,
            site,
            crate::disposition::occupation_draw_key(founded),
            location,
            spread,
        ))
    }

    /// Whether a **community** takes the initiative at all — spec §4.2a's
    /// **disposition** inhibition, the durable one. A community whose drawn
    /// `threat_response` (flee 0 ↔ stand 1) falls below `RAID_DISPOSITION_MIN`
    /// never raids, however strong it is on paper; `None` (a people with no
    /// authored disposition) is not vetoed, so a bake handed no data behaves
    /// exactly as it did before the gate existed.
    ///
    /// **It reads a community, not a people, and that is the campaign's point.**
    /// Until The Tolerance the argument was a `KindId` and the comparison was
    /// against `BakeConfig.disposition[people]` — a species constant, so every
    /// settlement of a people answered identically and warlikeness was a
    /// property of a *kind*. It now compares [`Community::disposition`], drawn
    /// per settlement from that people's authored mean and spread and keyed on
    /// the settlement's own `(site, founded year)`. Two settlements of one
    /// people can answer differently; a people is a distribution over stances,
    /// not a stance.
    ///
    /// Taking the value rather than an index is deliberate: the roll-downhill
    /// path fires on a community that has already *closed* (a remnant on the
    /// road has no live index), so its disposition travels down
    /// [`Bake::relocate`] as the value it was opened with. A band carries the
    /// mind of the settlement it lost, which is the only reading of "this
    /// people, here" available once the seat is gone.
    ///
    /// **What this gate is still NOT.** Spec D5 writes warlikeness as
    /// `f(structural pressure, drawn disposition, grid/group quadrant)`; only
    /// the middle term is built. The pressure term is deliberately excluded —
    /// [`Bake::pressure_of`] already varies per settlement, so folding it in
    /// would leave between-settlement variance standing when dispersion is
    /// zeroed and would break the campaign's own mutation proof, besides
    /// confounding preregistered H2 (which attributes that variance to the
    /// authored dispersion). Spec D5's "already-designed structural term" —
    /// the strife field — is in any case **not reachable from the bake**: it
    /// exists only as `report.byproducts.strife` off `demography_report_from`,
    /// on the Lab readout path, downstream of the history it would have to
    /// inform. The grid/group quadrant is deferred on weaker grounds: its
    /// inputs (`sociality`, `in_group_radius`) are per-*people* constants, so
    /// it would add no between-settlement variance and threatens nothing — it
    /// is simply an unpreregistered behavioural change with no measurement
    /// attached, and D6 is adopted as documentation on `SocietyVector` instead.
    ///
    /// It buys an asymmetric aversion structure for free: A declines B while B
    /// raids A, because each community gates on its own draw and never on the
    /// pair. Like [`Bake::has_spoils`] it is a conjunct in both candidate
    /// loops — a timid community driven off its land pioneers rather than
    /// rolling over a holder it could have beaten.
    fn takes_the_initiative(&self, disposition: Option<f64>) -> bool {
        match disposition {
            None => true,
            Some(d) => d >= RAID_DISPOSITION_MIN,
        }
    }

    /// The share of its surplus a people hides from its patron — spec §4.2's
    /// **concealment** term, the subordinate's half of the negotiation.
    ///
    /// The information asymmetry costs nothing because it is already
    /// structural: the dominant assesses what it can SEE (the subordinate
    /// cell's `eff_capacity`, which land tax has always been levied on) while
    /// the subordinate hands over out of what it HAS (`population`). Those are
    /// two different numbers already, and the gap is the subordinate's to
    /// manage.
    ///
    /// An insular people (`in_group_radius` → 0) hides most: it draws its
    /// in-group tightly and an outsider sees little of what it holds. An
    /// expansive one (→ 1) hides nothing. A people with no authored society —
    /// every non-`Settled` kind carries none — conceals nothing, so a bake
    /// handed no data behaves exactly as it did before the term existed.
    ///
    /// The radius is a ratio in `[0, 1]` by construction (`SocietyVector`), and
    /// is clamped anyway: a radius above 1 would otherwise give a *negative*
    /// concealment — a subordinate that remits more than it produced.
    ///
    /// **Non-finite fails safe** (T4 review, Minor 1): `SocietyVector.
    /// in_group_radius` is a bare `pub f64` whose `[0, 1]` contract is a doc
    /// comment only, so nothing structurally prevents NaN or an infinity from
    /// reaching here (today's authored values make it unreachable in
    /// practice). `f64::clamp` returns NaN for a NaN input, and `collect_
    /// tribute`'s cap is `assessment.min(surplus * (1.0 - conceal))` —
    /// `f64::min` DISCARDS a NaN operand rather than propagating it, so an
    /// unguarded NaN concealment would silently void the growth cap and let a
    /// remittance reach into the standing stock, the one thing spec §4.2
    /// forbids. [`Bake::takes_the_initiative`] already fails safe on a
    /// non-finite disposition (its `>=` comparison is simply `false` for
    /// NaN); this mirrors that by treating any non-finite radius as
    /// concealing nothing rather than voiding the cap.
    fn concealment_of(&self, people: KindId) -> f64 {
        match self.in_group_radius.get(&people) {
            None => 0.0,
            Some(&r) if !r.is_finite() => 0.0,
            Some(&r) => (1.0 - r.clamp(0.0, 1.0)) * CONCEAL_MAX,
        }
    }

    /// How far ahead a people plans when it holds somebody — its authored
    /// `MindVector.time_horizon` (immediate 0 ↔ generational 1), read as the
    /// **discount rate** it applies to its vassal's future (spec §4.3a).
    ///
    /// A people with no authored psyche is read at `NEUTRAL_HORIZON` rather
    /// than at zero: this axis has no "unaffected" value, and zero is the
    /// cruellest patron in the family, not an absent one. A non-finite
    /// horizon falls back the same way — the same fail-safe direction
    /// [`Bake::takes_the_initiative`] and [`Bake::concealment_of`] already
    /// take — and an out-of-range one is clamped into `[0, 1]`, so no authored
    /// value can push a setpoint outside the band between the floor and
    /// maximum sustainable yield.
    fn horizon_of(&self, people: KindId) -> f64 {
        match self.time_horizon.get(&people) {
            None => NEUTRAL_HORIZON,
            Some(&h) if !h.is_finite() => NEUTRAL_HORIZON,
            Some(&h) => h.clamp(0.0, 1.0),
        }
    }

    /// The horizon a patron of this people actually applies to ONE relation,
    /// given how many **other** vassals it holds — spec §4.3c's portfolio
    /// effect, the campaign's second and structural source of strategy
    /// variation.
    ///
    /// ```text
    /// effective = horizon / (1 + others / PORTFOLIO_HALVING)
    /// ```
    ///
    /// `others` is the count of the patron's *further* holdings, never the
    /// relation in hand: a patron with a single vassal has no alternative to
    /// it and applies its authored horizon unchanged, which keeps the anchor of
    /// this rule exactly on the §4.3a behaviour it modulates. At the raid site
    /// the same reading is "the vassals I already hold" — the alternatives to
    /// the one I am contemplating — so both call sites pass the same quantity.
    ///
    /// **The shape is a hyperbola, and the choice is not arbitrary.** Three
    /// properties are required and this is the simplest form with all of them:
    ///
    /// * **Monotone** — strictly decreasing in `others`, so more holdings
    ///   always means a shorter horizon and the rule can never reverse itself
    ///   on some range of the count.
    /// * **Bounded and sign-safe** — the result lies in `(0, horizon]` for
    ///   every count, so no patron can wrap into a negative or nonsensical
    ///   horizon however many vassals it accumulates. A linear
    ///   `horizon × (1 − others/K)` would go negative past `K` and would need a
    ///   clamp that then flattens the top of the distribution; this saturates
    ///   toward zero on its own.
    /// * **Diminishing** — the step from one holding to two moves the horizon
    ///   far more than the step from eight to nine, which is the right story:
    ///   the FIRST alternative is what makes a vassal expendable, and the
    ///   twentieth adds little.
    ///
    /// It also avoids the exponential's transcendental: this is one division,
    /// IEEE-exact and libm-free, so it is the same bits on every platform
    /// without going through `kernel::math` at all.
    ///
    /// **What the model does NOT have is marginality.** Spec §4.3c's image is
    /// an empire crueller to its *distant provinces* than to its core, but a
    /// one-level star carries no ordering over its points — no distance, no
    /// seniority, no rank — so the shortening applies uniformly across a
    /// patron's whole portfolio rather than picking out the marginal vassals.
    /// The aggregate the spec asks for (a large holder extracts harder) is
    /// produced; the within-portfolio gradient is not, and inventing an order
    /// to fake it would be authoring the cruelty rather than deriving it.
    ///
    /// Reads the patron's **people** and a **count taken from the relation
    /// table**, and nothing mutable — see [`Bake::collect_tribute`], which
    /// snapshots the counts before the pass for exactly that reason.
    fn effective_horizon(&self, people: KindId, others: usize) -> f64 {
        self.horizon_of(people) / (1.0 + others as f64 / PORTFOLIO_HALVING)
    }

    /// The stock a patron of this people steers its vassal toward — the
    /// setpoint the demand aims at (spec §4.3a), read off the vassal's cell
    /// (`eff`, what the patron can SEE) and the patron's own horizon.
    ///
    /// **This is renewable-resource economics, and the vassal's logistic
    /// growth makes the mapping exact.** The epoch increment is `GROWTH_RATE ×
    /// N × (1 − N/eff)`, whose peak — maximum sustainable yield — sits at
    /// `N = eff/2`. A patron maximising the *discounted stream* holds its
    /// vassal at that peak, so its relation persists indefinitely; one
    /// maximising *this epoch* strips the stock to the floor and is left with
    /// a husk. The horizon interpolates between them:
    ///
    /// ```text
    /// target = FARM_FLOOR + horizon × (eff/2 − FARM_FLOOR)
    /// ```
    ///
    /// Between the two ends lie the protection racket and the Danegeld: the
    /// family is **generated, not enumerated**. Extinction is then Clark's
    /// case — optimal only for the shortest-sighted holder — rather than a
    /// special-cased outcome.
    ///
    /// **`FARM_FLOOR` remains a floor, not an exemption.** On land too poor to
    /// carry `2 × FARM_FLOOR`, the interpolation would put the setpoint
    /// *below* the floor and a patron steering to it would reach through the
    /// one bound spec §8.3 does not permit crossing, so the result is raised
    /// back to the floor. The clamp binds only on marginal cells; on any cell
    /// whose capacity supports twice the floor the raw interpolation already
    /// sits above it.
    ///
    /// **The horizon read here is the EFFECTIVE one** ([`Bake::
    /// effective_horizon`]): a patron holding `others` further vassals aims
    /// lower at every one of them, so a busy patron's setpoint sits nearer the
    /// floor than a quiet patron of the same people (spec §4.3c). At
    /// `others == 0` the term vanishes and this is exactly §4.3a's rule.
    ///
    /// Reads the patron's **people**, the subordinate's **cell**, and a
    /// **count snapshotted before the collection pass** — none of them moving
    /// across that pass — so [`Bake::collect_tribute`]'s order-independence
    /// survives this term. Nothing here reads the patron's `stores` or
    /// `population`, and nothing reads the LIVE relation table, which is
    /// exactly the reach that would make the iteration order decide the
    /// outcome.
    fn target_stock(&self, people: KindId, others: usize, eff: f64) -> f64 {
        let horizon = self.effective_horizon(people, others);
        (FARM_FLOOR + horizon * ((eff / 2.0) - FARM_FLOOR)).max(FARM_FLOOR)
    }

    /// The smallest community a patron of this people will accept as a vassal,
    /// read off the vassal's cell (`eff`) and the patron's own horizon — spec
    /// §4.3b, the same discount rate applied one step earlier, to the decision
    /// to subordinate *at all*.
    ///
    /// **The measurement this exists to answer.** 45.7% of relations opened on
    /// a community sitting at `DAUGHTER_POP`, which is always below
    /// [`crash_basin_fraction`] × `eff`: the opening demand exceeded everything
    /// the vassal could grow, so the patron was eating the stock from the first
    /// collection. Those relations were doomed at conception, whatever
    /// [`Bake::target_stock`] later steered them toward — the setpoint decides
    /// where a *farmable* vassal rests, and says nothing about one that was
    /// never farmable.
    ///
    /// ```text
    /// min_vassal = horizon × crash_basin_fraction() × eff
    /// ```
    ///
    /// The two ends are the whole content of the rule: an **immediate** patron
    /// (horizon 0) demands nothing of a target's size and takes anything it can
    /// beat, while a **generational** one (horizon 1) insists on a going
    /// concern — a vassal already clear of the crash basin, which is the
    /// weakest condition under which the relation it is opening can pay for
    /// itself out of growth. The low root is therefore a *ceiling* on this
    /// gate, not a floor: no patron ever declines a community that was actually
    /// farmable, so the rule can trim the doomed openings without making
    /// subordination inert. The anchor at zero is likewise exact rather than
    /// nominal — at horizon 0 the term vanishes and the shipped dominance and
    /// no-spoils vetoes are again the only conditions, so this cannot quietly
    /// become a floor on vassal size in general.
    ///
    /// **The whole roster is reachable, and this bound has now gone stale
    /// twice.** It was authored as *three* — bugbear (horizon 0.3), hobgoblin
    /// (0.5), kobold (0.8) — which was correct on the four-people roster of the
    /// time, goblin being the one vetoed out of raiding by
    /// `RAID_DISPOSITION_MIN`. Two things have invalidated it since:
    ///
    /// 1. **The Vacancy added the gnoll**, whose `threat_response` of 0.85
    ///    clears the gate easily, so its horizon of **0.2 has been reachable
    ///    patron-side ever since** — and is *shorter* than bugbear's. The bound
    ///    became four values and the comment was never updated. (The Generalist
    ///    then added human at `threat_response` 0.5, which the gate vetoed, so
    ///    that campaign left the count at four.)
    /// 2. **The Tolerance** made `threat_response` a per-settlement draw, so no
    ///    people is vetoed *as such*: a particular goblin or human community
    ///    can clear the gate and take a vassal.
    ///
    /// All **six peoples** therefore reach patronhood now — gnoll 0.2, bugbear
    /// 0.3, goblin 0.5, hobgoblin 0.5, human 0.75, kobold 0.8 — admitting
    /// vassals above `0.029 × eff`, `0.044 × eff`, `0.073 × eff`,
    /// `0.073 × eff`, `0.110 × eff` and `0.117 × eff` respectively. That is
    /// **five distinct THRESHOLDS**, not six: this gate is linear in the
    /// horizon (`effective_horizon × crash_basin_fraction() × eff`), so goblin
    /// and hobgoblin — authored at the same 0.5 — map to the same cut. Five
    /// distinct cuts of one band, where it was four before this campaign and
    /// three when the comment was written, and still not the continuum the
    /// formula could express.
    ///
    /// **Do not read "five" here as [`BakeConfig::time_horizon`]'s count.**
    /// That doc counts how many peoples sit *away from* `NEUTRAL_HORIZON`
    /// (four: gnoll, bugbear, human, kobold), which is a different question
    /// from how many distinct thresholds the roster produces (five, the count
    /// above). Both are stated in full in both places so neither has to be
    /// re-derived from the other. Said here so it is not rediscovered as a
    /// surprise.
    ///
    /// **The horizon read here is the EFFECTIVE one** ([`Bake::
    /// effective_horizon`]), and the composition with spec §4.3c is a real
    /// consequence rather than an oversight: a patron that already holds
    /// `others` vassals applies a shortened horizon, which lowers its own
    /// minimum, so **a busy patron takes vassals a quiet one of the same people
    /// would decline**. That is the same sentence §4.3c is written in — an
    /// empire acquiring provinces it will farm carelessly — read at the moment
    /// of acquisition instead of the moment of collection, and it is measured
    /// rather than special-cased away.
    ///
    /// Reads the raider's **people**, the target's **cell**, and how many
    /// vassals the raider holds — the last read ONCE, above the candidate walk
    /// in [`Bake::maybe_raid`], since nothing in that walk mutates the relation
    /// table. It reads no population of the patron's and no stores, so it
    /// cannot make the candidate scan's outcome depend on the order of the
    /// walk, and it consumes no draw.
    fn min_vassal(&self, people: KindId, others: usize, eff: f64) -> f64 {
        self.effective_horizon(people, others) * crash_basin_fraction() * eff
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
        founded_from: Founding<BakeId>,
        lineage: Option<BakeId>,
        tech_offset: f64,
    ) -> usize {
        let id = self.mint();
        let lineage = lineage.unwrap_or(id);
        let tech = tech_for(year + tech_offset);
        let record = BakeOccupation {
            core: Occupation {
                people,
                site,
                founded: year,
                ended: None,
                peak_population: population.round() as u32,
                tech,
                function: Function::Agrarian,
                deity: None,
                tongue: None,
                cause: None,
                notability: Notability::Common,
            },
            community: id,
            lineage,
            founded_from,
            ended_by: Ended::Nature,
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
            disposition: self.drawn_disposition(people, site, year),
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
    ///
    /// **A relocation is not a death, and the difference is made one line
    /// earlier** (spec §4.3e). A community that closes here and reopens
    /// elsewhere as one movement has already had its *portfolio* lifted by
    /// [`Bake::lift_portfolio`], so the second removal below finds nothing left
    /// to remove and the carry re-keys it onto the new seat. Its own
    /// obligation is **not** lifted — the continuity is role-asymmetric — so
    /// the first removal below still fires, and a vassal that flees leaves its
    /// patron behind. Every *other* close — Famine, a remnant lost on the road,
    /// annihilation — reaches this line with everything still in the table and
    /// forfeits it all, exactly as before. So this stays the single
    /// unconditional dissolution point: preserving is something a caller does
    /// deliberately, never something this method decides.
    fn close(&mut self, idx: usize, year: f64, cause: CauseOfEnd, ended_by: Ended<BakeId>) {
        let c = &mut self.communities[idx];
        c.alive = false;
        let site = c.site;
        let rec = c.record;
        self.records[rec].core.ended = Some(year);
        self.records[rec].core.cause = Some(cause);
        self.records[rec].ended_by = ended_by;
        // Only free the cell if THIS community is the one indexed there.
        if self.node_index.get(&site) == Some(&idx) {
            self.node_index.remove(&site);
        }
        // …and it is party to no relation, as subordinate or as patron.
        self.tribute.remove(&idx);
        self.tribute.retain(|_, t| t.patron != idx);
    }

    /// Lift the vassals `idx` holds off the live table and hand them back, so
    /// a caller that is about to `close` this community **as one half of a
    /// relocation** can re-key them onto the seat it reopens at (spec §4.3e).
    ///
    /// Called immediately before [`Bake::close`], whose own dissolution then
    /// finds this portfolio already gone. That is the whole distinction the
    /// mechanism turns on: **relocation lifts, death does not.** A community
    /// lost to Famine, lost on the road, or annihilated still forfeits its
    /// vassals, because nobody lifted them first.
    ///
    /// **`idx`'s own obligation is deliberately NOT lifted** — spec §4.3e's
    /// asymmetry. It stays in the table, so the `close` one line later
    /// dissolves it exactly as it would for a death, and a vassal that flees
    /// arrives free. Leaving it in place (rather than lifting and discarding
    /// it) keeps `close` the single dissolution point in this file: preserving
    /// is something a caller does deliberately, and everything a caller does
    /// not lift is dissolved there.
    ///
    /// Deterministic: the portfolio is collected in `BTreeMap` key order and
    /// the table is only ever *shrunk* here, so the lift cannot depend on when
    /// it runs.
    fn lift_portfolio(&mut self, idx: usize) -> CarriedPortfolio {
        let vassals: Vec<(usize, Tribute)> = self
            .tribute
            .iter()
            .filter(|(_, t)| t.patron == idx)
            .map(|(&sub, &t)| (sub, t))
            .collect();
        for (sub, _) in &vassals {
            self.tribute.remove(sub);
        }
        CarriedPortfolio { vassals }
    }

    /// Re-key a carried portfolio onto `new_idx`, the seat a relocating
    /// community has just reopened at — spec §4.3e's **wounded patron**. Each
    /// relation is the same relation: its assessment and its remembered
    /// population travel, so a patron that is driven off its land arrives still
    /// holding its vassals' obligation, having lost only the population the war
    /// took and the hoard its old community's closure destroyed. What this
    /// creates is the state the model previously had no room for — a patron
    /// that is *weakened* rather than dead — which is what spec §4.3d's revolt
    /// needs in order ever to fire.
    ///
    /// **`since` does NOT travel: this lord's tenure begins at `year`.** The
    /// obligation continues, but the patron on the far side of a relocation is
    /// a *new community with a new [`BakeId`]* — `open` minted it moments
    /// ago — and [`TributeRelation::since`] is documented as when the CURRENT
    /// patron took the relation over, not when the subordinate first began
    /// paying somebody (a patronage transfer already re-stamps it for exactly
    /// this reason). Carrying the old date forward made the emitted
    /// `pays-tribute-to` fact assert a relationship that predated one of its
    /// own named parties: on seed 42, 22 of 164 facts were dated up to 675
    /// years before the patron entity they name was founded (final review,
    /// Important 1). Role-asymmetrically so, because a fleeing vassal drops its
    /// relation and only the patron side survives a move.
    ///
    /// The alternative — leaving `since` alone and clamping at the emit
    /// boundary to `max(since, both foundings)` — was rejected: it repairs the
    /// one consumer that exists today while leaving the impossible date in
    /// `History::tribute`, which is public and which the next lab metric or
    /// chronicle reader would take at face value, and it makes `since` mean one
    /// thing in the bake and another in the ledger. Fixing it here keeps a
    /// single meaning everywhere.
    ///
    /// **Nothing in the mechanism reads `since`** — it is carried for emission
    /// alone — so this changes no rule, no draw and no census count; it changes
    /// only the day the surviving facts are stamped with.
    ///
    /// **Only the patron role arrives here**, because only the patron role is
    /// lifted ([`Bake::lift_portfolio`]). A relocating community's own
    /// obligation was left behind to be dissolved by its `close`, so a fleeing
    /// vassal is genuinely gone from its relation rather than merely at a new
    /// address (spec §4.3e's asymmetry).
    ///
    /// **A vassal that is not alive is dropped.** Two cases reach that: it
    /// genuinely died, and it is itself mid-relocation (a patron that conquers
    /// its own vassal's cell drives that vassal onto the road, and the road has
    /// not ended when this re-key runs). Dropping is the safe reading of both —
    /// `tribute` holds community *indices*, so re-keying onto a closed
    /// community would leave a dangling index, the silent corruption spec §4.4
    /// exists to forbid.
    ///
    /// **One-level stars survive the re-key** (spec §4.4). Each install is
    /// checked against the two shapes that would introduce depth — a
    /// subordinate that has since been taken over, and one that has since
    /// become a patron itself — so the invariant is enforced here rather than
    /// inherited by argument. The third shape, a *patron* that has become
    /// somebody's vassal, cannot arise: `new_idx` was opened moments ago by
    /// every caller and so is party to nothing, and the asymmetry means no
    /// obligation is carried onto it either.
    ///
    /// Order-independent: every install is into a `BTreeMap` under a fixed key,
    /// and the guards read only liveness and membership, neither of which the
    /// loop's own inserts can flip for a later iteration (the portfolio's keys
    /// are distinct, and they all name `new_idx` as patron, so no install can
    /// make a later `sub` look like a patron).
    fn carry_portfolio_to(&mut self, new_idx: usize, carried: CarriedPortfolio, year: f64) {
        for (sub, terms) in carried.vassals {
            if sub == new_idx || !self.communities[sub].alive {
                continue; // gone, or still on the road: the relation does not survive it
            }
            if self.tribute.contains_key(&sub) || self.tribute.values().any(|t| t.patron == sub) {
                continue; // it has been taken over, or has become a patron: no depth
            }
            self.tribute.insert(
                sub,
                Tribute {
                    patron: new_idx,
                    // This lord's tenure starts here: `new_idx` did not exist
                    // before `year`, so no fact naming it may be older.
                    since: year,
                    ..terms
                },
            );
        }
    }

    /// Open a new epoch: zero every community's growth buffer.
    ///
    /// The buffer must hold this epoch's increments and nothing else, so that
    /// [`Bake::collect_tribute`] can say which part of a remittance came out
    /// of the year's growth (spec §4.2). Since amendment 4 that split no
    /// longer *bounds* anything — see `Bake::epoch_growth` — so this clear is
    /// no longer the guard it was; it is what keeps the reported
    /// decomposition true. Rebuilt by length rather than filled in place so it
    /// stays parallel to `communities` even if a caller ever appends by some
    /// other route.
    fn begin_epoch(&mut self) {
        self.epoch_growth.clear();
        self.epoch_growth.resize(self.communities.len(), 0.0);
    }

    /// Each patron collects from each of its subordinates: it demands what its
    /// assessment says (set from the cell it can SEE) and receives what the
    /// subordinate hands over — paid from **that epoch's growth and, beyond it,
    /// from the standing stock down to the patron's own setpoint** (spec
    /// §4.2b, §4.3a):
    ///
    /// ```text
    /// target     = FARM_FLOOR + horizon × (eff/2 − FARM_FLOOR)   // ≥ FARM_FLOOR
    /// bleed      = max(0, stock − target)
    /// harvest    = max(0, surplus − max(0, target − stock))
    /// remittance = min(assessment, (harvest + bleed) × (1 − concealment))
    /// ```
    ///
    /// **Where the line sits is the patron's character** ([`Bake::
    /// target_stock`]): a generational people steers its vassal to maximum
    /// sustainable yield and holds a relation indefinitely; an immediate one
    /// steers it to the floor and strips. `harvest + bleed` is exactly what
    /// stands above the setpoint after this epoch's growth — the split names
    /// which part of the take came out of the year's increment and which out
    /// of the standing stock, and the two are separated here rather than
    /// summed straight off `population` because the increment is already IN
    /// `population` by the time collection runs, which is what made the
    /// pre-amendment expression reach through its own floor.
    ///
    /// **Amendment 3 reversed the earlier rule here, and the reversal is the
    /// point.** A remittance used to be capped at the epoch's growth increment
    /// — milk, never kill — which guaranteed `population_after ≥
    /// population_at_epoch_start` and so made the health signal §4.3 feeds back
    /// on non-negative from tribute BY CONSTRUCTION: the demand could ease only
    /// when war, famine or climate hurt the vassal, never because the patron
    /// over-extracted, and spec §1's over-extract → collapse → relax cycle did
    /// not close inside the mechanism. A patron demanding more than the surplus
    /// now genuinely shrinks its vassal, the signal goes negative from tribute
    /// alone, the demand eases, and the vassal recovers.
    ///
    /// `FARM_FLOOR` is a floor and not an exemption: a vassal may be bled
    /// toward it, never through it, so tribute alone still cannot end a
    /// community (spec §8.3, restated).
    ///
    /// What is handed over is net of the subordinate's **concealment** (spec
    /// §4.2's third term, [`Bake::concealment_of`]): an insular people hides
    /// more of what it owes from an outsider. Concealment scales the payment
    /// only, so it can only ever lower a remittance — and therefore never
    /// endanger the floor.
    ///
    /// The remittance lands in `stores`, never in `population` (spec §4.2a):
    /// tribute is wealth, not bodies, and a patron whose winnings entered the
    /// pressure term would starve itself on its own success.
    ///
    /// Each collection then **corrects the demand** against what the visit
    /// found (spec §4.3): a vassal larger than the patron last saw it can bear
    /// more, one that has shrunk is being over-milked. The correction is
    /// multiplicative in the standing assessment and proportional to the
    /// *relative* change in the subordinate's population, clamped into
    /// `[0, eff_capacity × ASSESS_MAX]`. It runs for every live relation
    /// visited, including one that remitted nothing — a bad year is exactly
    /// the reading the patron most needs.
    ///
    /// Deterministic: the relation table is a `BTreeMap`, so the pass runs in
    /// subordinate-index order, and it is snapshotted to a `Vec` first so no
    /// entry's outcome can depend on a mutation made earlier in the same pass.
    /// Each entry reads only its own snapshotted relation, its own
    /// subordinate's population, and `epoch_growth` (frozen by the step loop
    /// that has already finished), and writes only its own key — so no entry
    /// can read what an earlier one wrote and the order is immaterial. That is
    /// a property of what this loop happens to read, not one anything
    /// enforces: a term that read the patron's `stores` — the obvious next
    /// reach, since `strength` does — would make the order decide the outcome,
    /// and the snapshot would then be load-bearing rather than
    /// belt-and-braces. (`era` is read only for the clamp's ceiling, which is
    /// a property of the subordinate's land.)
    ///
    /// # It returns whom it drove out, and does not act on it
    ///
    /// Spec §4.3d gives the subjugated two answers beyond concealment, and the
    /// epoch runs them **around** this method rather than inside it
    /// ([`bake`]'s loop is where the sequence is spelled out):
    ///
    /// ```text
    /// settle_revolts()          // 1. the vassals strong enough to refuse do
    /// collect_tribute()  -> …   // 2. the rest pay, and say who is leaving
    /// resolve_flights(…)        // 3. those who are leaving go
    /// ```
    ///
    /// **Revolt is settled BEFORE collection.** A vassal that can already
    /// out-muscle its patron by `RAID_MARGIN` is not milked on the way out:
    /// the strength comparison is a fact about the state the epoch hands over,
    /// and collecting first would credit a patron with a tribute nobody in the
    /// world was in a position to compel. It is also what keeps the two
    /// mechanisms independent — a revolting vassal never reaches the
    /// collection that could have produced a burden, so no event is ever both.
    ///
    /// **Flight is resolved AFTER collection, and is returned rather than
    /// executed**, for two reasons that both matter:
    ///
    /// - The burden a vassal leaves over is a quantity this pass *produces* —
    ///   the remittance actually handed over, as a share of the community that
    ///   handed it over — so it cannot be known one line earlier. The vassal
    ///   pays, learns the price of staying, and goes.
    /// - A flight **moves people**: it closes a community and relocates it,
    ///   which can evict a holder, cascade, and dissolve relations belonging
    ///   to other entries of this very pass. Executed inline it would make
    ///   each entry's outcome depend on where in the pass it was reached, and
    ///   — worse — it would end this method's standing property that
    ///   **nothing but tribute moves a population across a collection**. Every
    ///   test in this file that reads a remittance as a population difference
    ///   depends on that property; with relocation inside, a neighbour's
    ///   flight rolling over a vassal would read as a tribute of 30% of it.
    ///
    /// So the departures are returned, and [`Bake::resolve_flights`] is a
    /// separate step of the epoch.
    fn collect_tribute(&mut self, year: f64, era: &EraClimate) -> Vec<usize> {
        // Whom this epoch's collection drove out. Recorded inside the loop and
        // returned — see the note above.
        let mut fleeing: Vec<usize> = Vec::new();
        let relations: Vec<(usize, Tribute)> = self.tribute.iter().map(|(&s, &t)| (s, t)).collect();
        // How many vassals each patron holds, taken from the SNAPSHOT and not
        // from the live table (spec §4.3c). The count is what the portfolio
        // effect reads, and reading it live would be the exact fragility this
        // method's doc warns about: `self.tribute` is written inside the loop,
        // so a live count would make every relation's outcome a function of
        // where in the pass it was reached. (Today's writes replace a key with
        // the same patron and so cannot move a count — which is precisely the
        // kind of accident that stops being true one edit later.) Deterministic
        // container, and the whole map is built before a single mutation.
        let mut holdings: BTreeMap<usize, usize> = BTreeMap::new();
        for (_, rel) in &relations {
            *holdings.entry(rel.patron).or_insert(0) += 1;
        }
        for (sub, rel) in relations {
            // Cheap, and the failure it guards is silent. `close` dissolves
            // both directions of every relation a dying community was party to
            // (spec §4.4), so this should be unreachable — but a corpse taxed,
            // or a dead patron quietly enriched, would show up nowhere.
            if !self.communities[sub].alive || !self.communities[rel.patron].alive {
                continue;
            }
            let surplus = self.epoch_growth[sub].max(0.0);
            // What a greedy patron may reach into BEYOND the surplus: the stock
            // standing above the floor (spec §4.2b's `bleed` term). This is the
            // amendment — a demand larger than the increment now genuinely
            // shrinks its vassal, which is the only way this loop's own health
            // signal can go negative.
            //
            // Measured on the stock the epoch FOUND — `population` less this
            // epoch's surplus — and not on `population` itself, which by this
            // point already carries that surplus. Reading it off `population`
            // double-counts the increment and lets a remittance reach THROUGH
            // the floor by up to that increment; measured on this file's own
            // floor fixture rather than supposed, a vassal that began an epoch
            // at 3.157 was farmed to 1.746 under a `FARM_FLOOR` of 2.0. The
            // floor is a floor and not an exemption (spec §4.2b), so the
            // decomposition the spec states — this epoch's growth PLUS the
            // stock above the floor — is implemented with `bleed` read against
            // the stock. Their sum is then exactly what stands above the floor
            // after growth, so `population_after ≥ FARM_FLOOR` whenever the
            // stock reached the floor at all, and a stock already below it
            // (war, famine, crowding — never tribute) is taken no lower.
            let stock = self.communities[sub].population - surplus;
            // The line the reach stops at is the patron's SETPOINT, not the
            // bare floor (spec §4.3a's amendment 4). It is read off the
            // patron's people — its authored horizon — and the subordinate's
            // cell, the two things a patron brings to the relation and can
            // see; `target_stock` carries the derivation. Both are immutable
            // across a collection pass, so the order-independence this method
            // documents below survives the new term.
            let sub_eff = self.eff_capacity(era, self.communities[sub].site);
            let patron_people = self.records[self.communities[rel.patron].record]
                .core
                .people;
            // …and by how many OTHER vassals this patron holds (spec §4.3c):
            // the alternatives to this relation are what make it expendable, so
            // the relation in hand is excluded. A patron holding only this one
            // has no alternatives and applies its authored horizon unchanged.
            // The count came from the snapshot above, so it is the same
            // whatever order this pass runs in.
            let others = holdings.get(&rel.patron).copied().unwrap_or(1) - 1;
            let target = self.target_stock(patron_people, others, sub_eff);
            let bleed = (stock - target).max(0.0);
            // A vassal standing BELOW the setpoint is left to recover, not
            // merely taxed less: only the growth that carries it past the line
            // is harvested. Without this a below-target vassal hands over its
            // whole increment every epoch, is held exactly flat forever, and no
            // horizon can move it — which is the floor-pinned attractor the
            // investigation measured (79.2% of relations, frozen for the rest
            // of the bake).
            let harvest = (surplus - (target - stock).max(0.0)).max(0.0);
            // Concealment scales what is HANDED OVER, never what the cap is
            // measured against, so a hidden share is one the subordinate keeps
            // (spec §4.2) — and, since it can only ever LOWER a remittance, the
            // setpoint (and with it the floor beneath it) holds at any
            // concealment.
            let conceal =
                self.concealment_of(self.records[self.communities[sub].record].core.people);
            let remittance = rel.assessment.min((harvest + bleed) * (1.0 - conceal));
            // The burden: what was handed over as a share of the community
            // that handed it over, measured against the population the demand
            // was met out of — this epoch's growth included, since that
            // increment is what the vassal had when it paid. A share above
            // `FLIGHT_BURDEN` is one the vassal's own growth law could not
            // regrow at any stock, and it leaves (spec §4.3d). Recorded here
            // and acted on after the pass; a zero or negative population
            // cannot produce a burden and is left to the paths that own it.
            let paid_from = self.communities[sub].population;
            if paid_from > 0.0 && remittance > paid_from * FLIGHT_BURDEN {
                fleeing.push(sub);
            }
            self.communities[sub].population -= remittance;
            self.communities[rel.patron].stores += remittance;
            self.tally.tribute_collected += remittance;
            self.tally.tribute_collection_events += 1;
            // A no-op in today's call sequence — `grow` touched the patron at
            // this same `year` and this same population earlier in the epoch,
            // and collection moves `stores`, which `touch` does not read. Kept
            // for the same reason the subordination branch keeps its own
            // (`maybe_raid`): every mutation site in this bake records the
            // community it moved, and a future caller reaching collection by
            // another route must not silently skip that.
            self.touch(rel.patron, year);
            // The correction (spec §4.3). The patron reads the vassal it is
            // leaving against the one it last left: a RELATIVE change, so a
            // large people and a small one are read on the same scale, and
            // two-signed, so the demand can ease as well as climb. Zero when
            // the last reading was zero — a vassal that was not there to be
            // seen tells the patron nothing, and dividing by it would put a
            // NaN in the ledger.
            let now_pop = self.communities[sub].population;
            let signal = if rel.last_seen_population > 0.0 {
                (now_pop - rel.last_seen_population) / rel.last_seen_population
            } else {
                0.0
            };
            // Spec §4.5's divergence bound, applied where the assessment is
            // written: no patron may demand more than the subordinate's land
            // could ever produce.
            let ceiling = sub_eff * ASSESS_MAX;
            let next = (rel.assessment + signal * rel.assessment * ADAPT_RATE).clamp(0.0, ceiling);
            self.tribute.insert(
                sub,
                Tribute {
                    assessment: next,
                    last_seen_population: now_pop,
                    ..rel
                },
            );
        }
        fleeing
    }

    /// Execute the departures [`Bake::collect_tribute`] returned — spec
    /// §4.3d's flight, at the one place in the epoch where moving people
    /// cannot be mistaken for extraction.
    ///
    /// The list arrives in subordinate-index order (`BTreeMap` key order, the
    /// order the collection pass built it in), and is executed in that order.
    /// One flight can close another would-be leaver by cascading over it,
    /// which [`Bake::take_flight`] handles by checking the community is still
    /// alive; that is the whole of the coupling between entries, and it is why
    /// the order — while deterministic — is stated rather than left to be
    /// inferred.
    fn resolve_flights(&mut self, fleeing: Vec<usize>, era: &EraClimate, year: f64) {
        for sub in fleeing {
            self.take_flight(sub, era, year);
        }
    }

    /// Every vassal that has come to out-muscle its patron throws it off —
    /// spec §4.3d's **revolt**, and the second of the two answers the
    /// subjugated have beyond concealment.
    ///
    /// The test is the bake's own dominance test, read the other way round:
    /// `strength(vassal) > strength(patron) × RAID_MARGIN`. A relation is
    /// only ever imposed by a patron that cleared exactly that margin over its
    /// target, so a revolt requires the balance to have swung by `RAID_MARGIN`
    /// squared since — it is not the mirror of the raid rule loosened, it is
    /// the same threshold with the roles exchanged. Structural, total, and
    /// **draw-free**: nobody decides to revolt, it is a thing that has become
    /// true of two populations and two hoards.
    ///
    /// **Exactly one relation ends per revolt, and that is the whole design.**
    /// Freeing the patron's other vassals in the same breath is spec §9's
    /// deferred *collapse-release*, which frees a network by construction and
    /// would settle spec §5's headline by fiat. What happens here instead is
    /// that the patron loses this vassal's stream, so its `stores` decay
    /// without replacement, so its `strength` falls, so it becomes beatable —
    /// by its remaining vassals on some later epoch, and by third parties
    /// through the shipped raid rule. The chain is **emergent**: every link in
    /// it is a mechanism that already shipped, and none of them is reached
    /// from here.
    ///
    /// **Termination.** This pass only ever *removes* entries, from a snapshot
    /// taken before the first removal, so it visits each standing relation at
    /// most once and cannot re-enter. A revolt does change strengths' *usage*
    /// downstream (a poorer patron is a likelier target next epoch), but
    /// nothing within this epoch re-reads the table after the pass except the
    /// collection it precedes.
    ///
    /// Deterministic: `BTreeMap` key order over a `Vec` snapshot, and the
    /// comparison reads only populations, stores and tech — none of which this
    /// pass writes — so the outcome is the same whatever order it runs in.
    fn settle_revolts(&mut self) {
        let standing: Vec<(usize, usize)> = self
            .tribute
            .iter()
            .map(|(&sub, t)| (sub, t.patron))
            .collect();
        for (sub, patron) in standing {
            // The same cheap coherence guard the collection pass carries: a
            // dead party's relation should already be gone (spec §4.4), and a
            // corpse throwing off a corpse would show up nowhere.
            if !self.communities[sub].alive || !self.communities[patron].alive {
                continue;
            }
            if self.strength(sub) > self.strength(patron) * RAID_MARGIN {
                self.tribute.remove(&sub);
                self.tally.vassal_revolts += 1;
            }
        }
    }

    /// A vassal whose burden crossed `FLIGHT_BURDEN` walks off its cell —
    /// spec §4.3d's **flight**. It is *leaving*, not being driven off: the
    /// occupation closes as `CauseOfEnd::Migrated` by `Ended::Nature`, exactly
    /// as a climate migration does, and the event is tallied as a `migrated`
    /// and a `vassal_flights`, **never** as a `fled` — that counter means
    /// driven off by a raider, and pooling the two would corrupt the census
    /// the campaign reports.
    ///
    /// The move itself is the shipped [`Bake::relocate`] path, so a fleeing
    /// people takes the best home in the nearest ring that offers it one, on
    /// the same terms as any other homeless people: marginal vacant ground, or
    /// a holding it can beat. A flight can therefore displace somebody and
    /// cascade — a real second source of avalanches, recorded into the same
    /// histogram at the same place [`Bake::maybe_raid`] records its own.
    ///
    /// **A community too small to survive the road does not take it.** The
    /// same `pop × MIGRATE_SURVIVAL ≥ VIABLE_MIN` guard the climate migration
    /// carries applies here, and it is what keeps flight from becoming a
    /// tribute-driven extinction: a vassal bled onto `FARM_FLOOR` cannot flee,
    /// it stays and endures.
    ///
    /// **A flight ends the relation, and that is the whole of what makes it a
    /// flight.** Spec §4.3e's continuity is role-asymmetric: a relocating
    /// community keeps the vassals it holds and drops the lord it owes, so the
    /// leaver arrives free. Shipped role-blind first and measured — obligation
    /// that followed the runaway turned flight into a change of address, and
    /// pooled flights went 78 → 647, overwhelmingly the same communities
    /// leaving again every epoch because leaving discharged nothing. §4.3d
    /// calls flight relocating "rather than continue in that condition", which
    /// only means anything if leaving ends the condition.
    ///
    /// The patron is not compensated and does not pursue: it simply stops
    /// collecting, so its `stores` decay without replacement and it becomes
    /// beatable — the same emergent chain a revolt starts, reached by the
    /// vassal's feet instead of its strength.
    ///
    /// Returns whether the community actually left, so a caller can tell a
    /// declined road from an executed one. **Leaving and arriving are not the
    /// same event**: `vassal_flights` is tallied only where the road ended in a
    /// seat, so a vassal that walked off and found nothing is a `collapsed`
    /// death rather than a flight (see the tally site below).
    fn take_flight(&mut self, idx: usize, era: &EraClimate, year: f64) -> bool {
        // A cascade earlier in this same pass may already have closed it.
        if !self.communities[idx].alive {
            return false;
        }
        let (record, pop, lineage, offset, id, site, disposition) = {
            let c = &self.communities[idx];
            (
                c.record,
                c.population,
                c.lineage,
                c.tech_offset,
                c.id,
                c.site,
                // Read before the close below: the fleeing vassal takes the
                // mind of the seat it is walking off, having no other.
                c.disposition,
            )
        };
        let arriving = pop * MIGRATE_SURVIVAL;
        if arriving < VIABLE_MIN {
            return false; // too small to survive leaving: it endures instead
        }
        let people = self.records[record].core.people;
        // Close BEFORE relocating so the cell it is abandoning is free — a
        // people must not be able to flee onto its own site, and `close` frees
        // the cell only when this community is the one indexed there. Lift the
        // PORTFOLIO first — a leaver that was itself a lord keeps its own
        // vassals — while its own obligation is deliberately left in the table
        // for the `close` below to dissolve. That is what makes a flight an
        // escape rather than a change of address (spec §4.3e).
        let carried = self.lift_portfolio(idx);
        self.close(idx, year, CauseOfEnd::Migrated, Ended::Nature);
        match self.relocate(
            people,
            arriving,
            lineage,
            id,
            offset,
            site,
            era,
            year,
            0,
            carried,
            disposition,
        ) {
            // `resettled` is tallied inside `relocate`, at the branch that
            // reaches vacant ground; only the migration itself is added here.
            //
            // **`vassal_flights` is tallied on the SETTLED branches only**, so
            // that it stays the strict subset of `migrated` its own doc comment
            // claims (final review, Important 2). Tallied before the
            // `relocate` it would count a vassal that left and died on the
            // road — a death, recorded as `collapsed` below — as a departure,
            // inflating the flight count above the migration count it is
            // supposed to sit inside. Not observed on a live world; reachable
            // by construction wherever a fleeing vassal has nowhere admissible
            // to go, which is what `a_flight_with_nowhere_to_go_is_a_death_not_
            // a_departure` builds.
            Relocation::Settled { cascade: 0 } => {
                self.tally.migrated += 1;
                self.tally.vassal_flights += 1;
            }
            Relocation::Settled { cascade } => {
                self.tally.migrated += 1;
                self.tally.vassal_flights += 1;
                self.tally.record_cascade(cascade);
            }
            // It left and died on the road — nothing reachable was habitable
            // or beatable. The same reading `step_community`'s climate
            // migration takes when no refuge exists, and the same tally
            // `relocate` uses for a remnant lost deeper in a chain: a
            // community may not vanish from the world uncounted.
            Relocation::Lost => self.tally.collapsed += 1,
        }
        true
    }

    /// Update a community's peak population and monotone tech from its current
    /// state at `year`.
    fn touch(&mut self, idx: usize, year: f64) {
        let c = &mut self.communities[idx];
        let peak = c.population.round() as u32;
        let rec = c.record;
        if peak > self.records[rec].core.peak_population {
            self.records[rec].core.peak_population = peak;
        }
        let tech = tech_for(year + c.tech_offset);
        if tech > c.tech {
            c.tech = tech;
        }
        if c.tech > self.records[rec].core.tech {
            self.records[rec].core.tech = c.tech;
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
            let people = self.records[record].core.people;
            // A refuge is only a refuge for a band big enough to hold it: a
            // migrant whose arriving population would fall below `VIABLE_MIN`
            // starves on the road instead of refounding, exactly as `relocate`
            // rules for a displaced remnant. Without this the refound would
            // `open` at `peak_population == 0` — a peopleless settlement.
            match self.nearest_dest(era, site) {
                Some(dest) if pop * MIGRATE_SURVIVAL >= VIABLE_MIN => {
                    // A climate eviction is the third close-and-reopen in this
                    // file, and it is a MOVE: the people walks to a refuge and
                    // goes on being whatever it was. Its vassals travel with it
                    // and its own lord does not (spec §4.3e), exactly as on the
                    // raid and flight paths — the asymmetry is a property of
                    // relocation itself, not of any one way of being made to
                    // move, so it is applied identically at all five sites.
                    let carried = self.lift_portfolio(idx);
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
                    self.carry_portfolio_to(new_idx, carried, year);
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
    /// **Four skips guard the subordination branch**, all placed *after* the
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
    /// 4. **The target is too small for this patron to farm** — spec §4.3b's
    ///    horizon-aware gate ([`Bake::min_vassal`]): a patron with foresight
    ///    wants a going concern, an immediate one takes anything it can beat.
    ///    It gates who may be *taken*, never who may be conquered, and a
    ///    declined target is not a raid — nothing is tallied and nobody is
    ///    touched. It sits alongside (1)–(3) after the covet test for that
    ///    reason; moving it above would let a long-horizon raider decline
    ///    better *land*, which is a different rule entirely
    ///    (`the_size_gate_never_vetoes_an_eviction`).
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
        // Read once: it does not vary over the candidate walk, and the size
        // gate (spec §4.3b) is keyed on it.
        let raider_people = self.records[self.communities[raider].record].core.people;
        // The durable inhibition: a timid COMMUNITY never takes the initiative,
        // so it never enters the candidate loop at all. Its own drawn
        // disposition, not its people's authored constant (The Tolerance) —
        // this seat's neighbour of the same kind may well answer otherwise.
        if !self.takes_the_initiative(self.communities[raider].disposition) {
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
        // How many vassals this raider already holds — spec §4.3c's portfolio
        // effect, read at the moment of acquisition. Hoisted for the same
        // reason as the two bindings above: nothing in the candidate walk
        // mutates the relation table, so the count cannot vary across it, and
        // reading it once makes that structural rather than incidental.
        let raider_holds = self
            .tribute
            .values()
            .filter(|tr| tr.patron == raider)
            .count();
        // (target index, that cell's value, the target's strength, its cell,
        //  and how a raid on it would resolve)
        let mut best: Option<(usize, f64, f64, CellId, Spoil)> = None;
        for n in traversable_neighbors(self.cur(), raider_site) {
            let Some(&t) = self.node_index.get(&n) else {
                continue;
            };
            let t_val = self.eff_capacity(era, n);
            let t_str = self.strength(t);
            if raider_str <= t_str * defensibility(self.cur(), raider_site, n) * RAID_MARGIN {
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
            } else if self.communities[t].population
                < self.min_vassal(raider_people, raider_holds, t_val)
            {
                // (4) Too small to farm: a patron with foresight passes (spec
                // §4.3b, `Bake::min_vassal`). Not a raid — nothing is tallied,
                // nobody is touched; if this is the only candidate the raider
                // simply does not raid this epoch, which is what "declines"
                // has to mean for a rule that fires BEFORE any spoil is taken.
                continue;
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
                        // The reading the first correction will be measured
                        // against: what this patron saw when it took the
                        // relation. A transfer overwrites it, so an incoming
                        // patron starts from what IT sees rather than
                        // inheriting its predecessor's memory.
                        last_seen_population: self.communities[target].population,
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
                self.records[c.record].core.people,
                c.id,
                c.lineage,
                c.tech_offset,
            )
        };
        let (loser_people, loser_id, loser_lineage, loser_offset, loser_disposition) = {
            let c = &self.communities[target];
            (
                self.records[c.record].core.people,
                c.id,
                c.lineage,
                c.tech_offset,
                // The beaten lord rolls downhill carrying the mind of the seat
                // he just lost — read before the close below.
                c.disposition,
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
        //
        // Both sides are RELOCATING, not dying, so both lift the vassals they
        // hold before their close and re-key them at the far end (spec §4.3e).
        // Neither carries its own obligation: the continuity is
        // role-asymmetric, so a raider that was somebody's vassal buys its
        // freedom with the move, and so does the loser it drives off. The
        // raider's lift runs first so that a patron conquering its own
        // vassal's cell moves that entry out as the patron's portfolio rather
        // than leaving it to be found twice — one carrier, never two.
        let raider_carried = self.lift_portfolio(raider);
        let loser_carried = self.lift_portfolio(target);
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
        self.carry_portfolio_to(seat, raider_carried, year);
        self.touch(seat, year);

        // The displaced loser rolls downhill, still carrying its (reduced)
        // strength and its standing relations — the cascade. Its own former
        // site is now the raider's, so it relocates away from `prize`. **This
        // is the wounded patron** (spec §4.3e): a lord beaten off his land
        // reaches his new seat still holding his vassals' obligation, having
        // lost the war's share of his people and, with his old community, the
        // whole hoard that made him unbeatable.
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
            loser_carried,
            loser_disposition,
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
                    (self.records[c.record].core.people, c.lineage, c.tech_offset)
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
        seed,
        disposition: &cfg.disposition,
        disposition_spread: &cfg.disposition_spread,
        in_group_radius: &cfg.in_group_radius,
        time_horizon: &cfg.time_horizon,
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
        // its patron happened to be stepped before or after it. Spec §4.3d's
        // two vassal answers bracket it, and the bracket IS the rule: a vassal
        // strong enough to refuse refuses before it is milked, and one that
        // pays a burden it could never regrow leaves after it has paid.
        // `collect_tribute`'s own doc carries the argument for both positions.
        bake.settle_revolts();
        let fleeing = bake.collect_tribute(year, &era);
        bake.resolve_flights(fleeing, &era, year);
        year += cfg.epoch_years;
    }

    // 3. Close at `now`: alive records keep `ended = None`.
    let now = cfg.end_year;
    bake.tally.alive_at_now = bake.records.iter().filter(|r| r.core.is_alive()).count() as u64;
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
    use std::cmp::Ordering;

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

    #[test]
    fn approach_ease_sums_traversable_conductance_only() {
        use hornvale_topology::{ConnectionGraph, Edge, EdgeKind};
        let mut g = ConnectionGraph::new(4);
        g.add_edge(
            CellId(0),
            Edge {
                to: CellId(1),
                kind: EdgeKind::Adjacency,
                conductance: 0.25,
            },
        );
        g.add_edge(
            CellId(0),
            Edge {
                to: CellId(2),
                kind: EdgeKind::LandRoute,
                conductance: 0.75,
            },
        );
        // Ocean-touching adjacency is stored at exactly 0.0. Note this is not
        // a test of the `conductance > 0.0` filter: a zero-valued term
        // contributes nothing to the sum whether or not it is filtered out
        // first, so this edge's presence is here only to document the
        // ocean-touching case, not to exercise exclusion behaviour.
        g.add_edge(
            CellId(0),
            Edge {
                to: CellId(3),
                kind: EdgeKind::Adjacency,
                conductance: 0.0,
            },
        );
        assert_eq!(approach_ease(&g, CellId(0)), 1.0);
    }

    #[test]
    fn approach_ease_is_zero_for_an_isolated_cell() {
        use hornvale_topology::ConnectionGraph;
        let g = ConnectionGraph::new(2);
        assert_eq!(approach_ease(&g, CellId(0)), 0.0);
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
            seed: Seed(1),
            disposition: no_disposition(),
            disposition_spread: no_spread(),
            in_group_radius: no_radius(),
            time_horizon: strips_to_the_floor(),
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
        let r1_disposition = bake.communities[r1_idx].disposition;
        assert_eq!(r1_id, lineage, "genesis community is its own lineage root");

        // First migration: R1 closes and relocates to vacant land, founded
        // from R1's own id — which equals `lineage` here, so this move alone
        // can't distinguish the bug from the fix.
        bake.close(r1_idx, 100.0, CauseOfEnd::Migrated, Ended::Nature);
        let outcome1 = bake.relocate_holding_nothing(
            people,
            9.0,
            lineage,
            r1_id,
            0.0,
            CellId(5),
            &era,
            100.0,
            0,
            r1_disposition,
        );
        let r2_idx = match outcome1 {
            Relocation::Settled { cascade: 0 } => bake.communities.len() - 1,
            other => panic!("expected a direct settle onto vacant land: {other:?}"),
        };
        let r2_id = bake.communities[r2_idx].id;
        let r2_site = bake.communities[r2_idx].site;
        let r2_disposition = bake.communities[r2_idx].disposition;
        assert_eq!(
            bake.records[bake.communities[r2_idx].record].founded_from,
            Founding::From(r1_id)
        );

        // Second migration: R2 closes and relocates again. Its predecessor is
        // R2's OWN id — distinct from the lineage root (R1's id) — so this is
        // the case that catches the bug: the buggy code named `lineage`
        // (R1), the fix names R2.
        bake.close(r2_idx, 200.0, CauseOfEnd::Migrated, Ended::Nature);
        let outcome2 = bake.relocate_holding_nothing(
            people,
            8.0,
            lineage,
            r2_id,
            0.0,
            r2_site,
            &era,
            200.0,
            0,
            r2_disposition,
        );
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

    /// The `disposition_spread` map a hand-built [`Bake`] uses when the test is
    /// not about The Tolerance's draw: empty, so every people draws with spread
    /// 0 and every community's disposition is its people's authored location
    /// exactly. That is the pre-Tolerance behaviour, which is what the existing
    /// per-people conflict fixtures in this module were written against and
    /// still assert; the draw itself is exercised by the tests that override
    /// this map.
    fn no_spread() -> &'static BTreeMap<KindId, f64> {
        static NONE: BTreeMap<KindId, f64> = BTreeMap::new();
        &NONE
    }

    /// The `in_group_radius` map a hand-built [`Bake`] uses when the test is
    /// not about concealment: empty, so nobody hides anything (the same
    /// fail-open the composition root sees when a people carries no
    /// `SocietyVector`).
    fn no_radius() -> &'static BTreeMap<KindId, f64> {
        static NONE: BTreeMap<KindId, f64> = BTreeMap::new();
        &NONE
    }

    /// The `time_horizon` map a hand-built [`Bake`] uses when the test is not
    /// about extraction strategy: **every settling people at zero**, so every
    /// hand-built patron is a maximally short-sighted one whose setpoint is
    /// `FARM_FLOOR` exactly.
    ///
    /// Deliberately NOT the empty map. Absent, a people reads at
    /// `NEUTRAL_HORIZON`, whose setpoint sits at `(FARM_FLOOR + eff/2)/2` —
    /// far above the populations these fixtures open at — so every bleed and
    /// floor test would collect nothing and pass vacuously on a demand that
    /// never binds. Zero reproduces the pre-amendment-4 rule (strip to the
    /// floor) exactly, which is the behaviour those tests were written against
    /// and still assert. The fallback itself is covered separately, by
    /// `an_unauthored_patron_is_read_at_the_neutral_middle`, and the strategy
    /// axis by `a_generational_patron_leaves_a_markedly_larger_vassal_than_an_
    /// immediate_one`, which overrides this map on both arms.
    fn strips_to_the_floor() -> &'static BTreeMap<KindId, f64> {
        static IMMEDIATE: std::sync::LazyLock<BTreeMap<KindId, f64>> =
            std::sync::LazyLock::new(|| {
                ["goblin", "kobold", "hobgoblin", "bugbear"]
                    .into_iter()
                    .map(|k| (KindId(k), 0.0))
                    .collect()
            });
        &IMMEDIATE
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
        hand_bake_spread(
            graphs,
            capacity,
            river_prox,
            refugia,
            disposition,
            no_spread(),
        )
    }

    /// [`hand_bake`] with an explicit `disposition_spread` — the fixture for a
    /// test that IS about The Tolerance's per-settlement draw. Every other
    /// fixture goes through [`hand_bake`], which pins the spread at empty so
    /// the pre-campaign per-people behaviour is what those tests still see.
    fn hand_bake_spread<'a>(
        graphs: &'a [ConnectionGraph],
        capacity: &'a CellMap<f64>,
        river_prox: &'a CellMap<f64>,
        refugia: &'a CellMap<bool>,
        disposition: &'a BTreeMap<KindId, f64>,
        disposition_spread: &'a BTreeMap<KindId, f64>,
    ) -> Bake<'a> {
        Bake {
            graphs,
            cur_graph: 0,
            capacity,
            river_prox,
            refugia,
            seed: Seed(1),
            disposition,
            disposition_spread,
            in_group_radius: no_radius(),
            time_horizon: strips_to_the_floor(),
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

    #[test]
    fn a_cheaply_reached_holder_is_raided_and_a_dearly_reached_one_is_not() {
        // Two holders identical in every respect except the CONDUCTANCE of the
        // route reaching them from the raider. Only the cheaply-reached one may
        // be taken. Two defects this catches: the term wired to the ATTACKER's
        // side, and `from`/`to` transposed (the graph is mirrored, so a
        // transposition compiles and mostly works — it fails exactly when the
        // two cells' parallel-edge sets differ).
        use hornvale_kernel::ReferenceElevation;

        let geo = Geosphere::new(1);
        let raider_site = CellId(0);
        let neighbors = geo.neighbors(raider_site);
        assert!(
            neighbors.len() >= 2,
            "fixture precondition: the raider's cell needs at least two neighbours"
        );
        let easy = neighbors[0];
        let hard = neighbors[1];

        // A hand-built graph carrying ONLY the two approach edges the test
        // cares about — `traversable_neighbors` reads solely off
        // `raider_site`'s edge list, so nothing else needs to exist. Same
        // edge kind, wildly different conductance: the only difference
        // between the two approaches is how easy the road is.
        let mut graph = ConnectionGraph::new(geo.cell_count());
        graph.add_edge(
            raider_site,
            Edge {
                to: easy,
                kind: EdgeKind::Adjacency,
                conductance: 1.0e3,
            },
        );
        graph.add_edge(
            raider_site,
            Edge {
                to: hard,
                kind: EdgeKind::Adjacency,
                conductance: 1.0e-6,
            },
        );
        let graphs = vec![graph];

        // Uniform capacity everywhere: raider and both holders read the same
        // land value, so nothing but the approach conductance can decide
        // which holder falls.
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

        let mut bake = hand_bake(&graphs, &capacity, &river_prox, &refugia, no_disposition());

        // Population and tech are pinned identical between the two holders
        // (both KindId("kobold"), both opened at year 0 with no tech
        // offset) — the fixture's whole point is that conductance is the
        // only axis left to decide the outcome.
        let raider = bake.open(
            KindId("goblin"),
            raider_site,
            0.0,
            15.0,
            Founding::Genesis(raider_site),
            None,
            0.0,
        );
        let easy_idx = bake.open(
            KindId("kobold"),
            easy,
            0.0,
            10.0,
            Founding::Genesis(easy),
            None,
            0.0,
        );
        let hard_idx = bake.open(
            KindId("kobold"),
            hard,
            0.0,
            10.0,
            Founding::Genesis(hard),
            None,
            0.0,
        );

        // Precondition, so a failure here points at the fixture rather than
        // the mechanism: the raider must clear the easy holder's threshold
        // but not the dear one's, computed from the SAME `defensibility` the
        // mechanism itself reads (not a hand-rolled approximation of it).
        let raider_str = bake.strength(raider);
        let easy_str = bake.strength(easy_idx);
        let hard_str = bake.strength(hard_idx);
        let easy_threshold = easy_str * defensibility(bake.cur(), raider_site, easy) * RAID_MARGIN;
        let hard_threshold = hard_str * defensibility(bake.cur(), raider_site, hard) * RAID_MARGIN;
        assert!(
            raider_str > easy_threshold && raider_str <= hard_threshold,
            "precondition: raider strength ({raider_str}) must clear the easy \
             threshold ({easy_threshold}) but not the dear one ({hard_threshold}), \
             or the split below proves nothing"
        );

        bake.maybe_raid(raider, &era, 0.0);

        assert_eq!(
            bake.tribute.get(&easy_idx).map(|tr| tr.patron),
            Some(raider),
            "the cheaply-reached holder must be taken, by the raider"
        );
        assert!(
            !bake.tribute.contains_key(&hard_idx),
            "the dearly-reached holder must be left alone"
        );
    }

    /// A patron on prime land beside a subordinate on prime land, with the
    /// relation already standing and its assessment set far above anything the
    /// subordinate's land could produce. The oversized assessment is what makes
    /// the collection tests *discriminating*: under spec §4.2b's amended rule
    /// the remittance is then exactly the epoch's growth increment PLUS the
    /// stock standing above `FARM_FLOOR`, so the subordinate is bled onto the
    /// floor exactly — and under a rule that ignored the floor it would be
    /// bled through it, to nothing.
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
                last_seen_population: bake.communities[sub].population,
            },
        );
        (bake, patron, sub)
    }

    #[test]
    fn a_patron_accumulates_stores_without_its_pressure_rising() {
        // Spec §4.2b + §4.2a, the two halves of the slice's central claim:
        //   (a) the remittance is paid out of THIS epoch's growth increment
        //       AND the stock standing above `FARM_FLOOR` — so a patron
        //       demanding more than the surplus bleeds its vassal down onto
        //       the floor exactly, and no further. (Under the superseded cap
        //       this half read "milked back to exactly where it began the
        //       epoch, and no further"; amendment 3 reversed it, because a
        //       vassal that cannot shrink cannot emit a negative health signal
        //       and the cycle then never closes inside the mechanism.)
        //   (b) it lands in the patron's `stores`, never its `population`, so
        //       the patron's crowding pressure is bit-for-bit unmoved. A
        //       successful extractor that fed its winnings into `population`
        //       would drive itself into Famine, and the readout would report
        //       "accumulation does not chain" when the truth was self-harm.
        //       **This half is untouched by the amendment** and stays.
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
        bake.collect_tribute(0.0, &era);

        let takeable = increment + (sub_before_growth - FARM_FLOOR);
        assert_eq!(
            bake.communities[patron].stores.to_bits(),
            takeable.to_bits(),
            "the patron must receive exactly this epoch's growth increment ({increment}) plus \
             the stock standing above the floor ({sub_before_growth} - {FARM_FLOOR}) = \
             {takeable}: got {}",
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
            (bake.communities[sub].population - FARM_FLOOR).abs() < 1.0e-9,
            "a fully-bled subordinate is left on the floor exactly, never below it: {} vs \
             {FARM_FLOOR} (it began the epoch at {sub_before_growth})",
            bake.communities[sub].population
        );
        assert!(
            bake.communities[sub].population < sub_before_growth,
            "…and it ends the epoch BELOW where it began it ({} vs {sub_before_growth}) — the \
             bleed is what lets the health signal go negative from tribute alone (spec §4.2b)",
            bake.communities[sub].population
        );
        assert_eq!(
            bake.tally.tribute_collected.to_bits(),
            takeable.to_bits(),
            "the run total must count what actually moved"
        );
        assert_eq!(
            bake.tally.tribute_collection_events, 1,
            "one live relation collected once: the event counter (T4 review, \
             Important 2) must count the visit, not the wealth"
        );
    }

    #[test]
    fn a_vassal_at_its_patrons_setpoint_hands_over_nothing() {
        // **Repointed at T5c, because its old claim stopped being one.** This
        // was `last_epochs_growth_is_never_taxed_twice`: the growth buffer is
        // strictly per-epoch, and a stale increment left standing used to be
        // re-taxed every epoch forever, so this was the one test that caught a
        // missing `begin_epoch`. Amendment 4 (spec §4.3a) ended that: the take
        // is `population − target` however it is decomposed, so the buffer's
        // staleness cannot move a remittance and suppressing the clear now
        // reddens nothing — mutation-verified in that direction rather than
        // supposed. Renaming it is the honest option; leaving a green test
        // standing for a property it no longer tests is the failure mode this
        // campaign has already been caught by four times.
        //
        // What the same fixture and the same assertions DO bind, and bind
        // sharply, is the setpoint's own resting state. This fixture's patron
        // is a maximally short-sighted one (`strips_to_the_floor`), so its
        // target is `FARM_FLOOR`; its oversized assessment bleeds the
        // subordinate onto that target in the first epoch. In the second — a
        // bad year, no growth at all — the vassal stands exactly ON the
        // setpoint, and a patron that steers toward a setpoint must take
        // NOTHING from a vassal already sitting on it. A rule that reached
        // past its own target (or that treated "nothing to take" as "take what
        // is there") moves wealth here, and both assertions redden.
        //
        // The between-epochs survival check is load-bearing rather than
        // decorative: it is the setpoint (here `FARM_FLOOR`) that leaves a bled
        // subordinate standing at all, and without it this subordinate would be
        // drained to nothing in the FIRST epoch, after which "at the setpoint ⇒
        // no tribute" would hold vacuously and the second epoch would prove
        // nothing.
        let (geo, graphs, capacity, river_prox, refugia, era) = cascade_world(|_| RICH);
        let (mut bake, patron, sub) = tribute_pair(&geo, &graphs, &capacity, &river_prox, &refugia);

        bake.begin_epoch();
        let pressure = bake.pressure_of(sub, &era);
        bake.grow(sub, &era, 0.0, pressure);
        bake.collect_tribute(0.0, &era);
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
        bake.collect_tribute(25.0, &era);

        assert_eq!(
            bake.communities[patron].stores.to_bits(),
            stores_after_one.to_bits(),
            "an epoch with no growth yields no tribute from a subordinate already standing on \
             its patron's setpoint: {} vs {stores_after_one}",
            bake.communities[patron].stores
        );
        assert_eq!(
            bake.communities[sub].population.to_bits(),
            sub_after_one.to_bits(),
            "a subordinate that did not grow, and holds nothing above its patron's setpoint, \
             must hand over nothing at all"
        );
        assert_eq!(
            bake.tally.tribute_collection_events, 2,
            "T4 review, Important 2: the event counter must count the VISIT, \
             not the wealth — it must still climb in the zero-growth second \
             epoch even though nothing was collected in it, or rate and \
             volume cannot be told apart from a run total alone"
        );
    }

    #[test]
    fn an_insular_subordinate_remits_less_than_an_expansive_one() {
        // Spec §4.2's third term: the dominant taxes what it can SEE
        // (`eff_capacity`), the subordinate holds what it HAS (`population`),
        // and **concealment is the gap the subordinate controls**. An insular
        // people (`in_group_radius` → 0) hides more from outsiders than an
        // expansive one.
        //
        // The two arms differ in EXACTLY ONE input — the subordinate people's
        // authored `in_group_radius` — so nothing but concealment can explain
        // a difference between them. Same world, same pair, same assessment,
        // same growth increment, same stream.
        //
        // Non-vacuity is asserted, not assumed: if concealment were applied to
        // the assessment cap instead of the surplus (or if both arms collected
        // nothing at all) a bare "insular remits less" could pass on two
        // zeros, so both arms are required to have actually moved wealth.
        let (geo, graphs, capacity, river_prox, refugia, era) = cascade_world(|_| RICH);
        // `tribute_pair`'s subordinate is the kobold. Only its entry differs.
        let expansive: BTreeMap<KindId, f64> = [(KindId("kobold"), 1.0)].into_iter().collect();
        let insular: BTreeMap<KindId, f64> = [(KindId("kobold"), 0.0)].into_iter().collect();

        // (remitted, what the subordinate was left holding) per arm.
        let mut arms: Vec<(f64, f64)> = Vec::new();
        for radius in [&expansive, &insular] {
            let (mut bake, patron, sub) =
                tribute_pair(&geo, &graphs, &capacity, &river_prox, &refugia);
            bake.in_group_radius = radius;
            bake.begin_epoch();
            let before = bake.communities[sub].population;
            let pressure = bake.pressure_of(sub, &era);
            bake.grow(sub, &era, 0.0, pressure);
            let increment = bake.communities[sub].population - before;
            assert!(
                increment > 0.0,
                "precondition: the subordinate must actually have grown this \
                 epoch — with no surplus there is nothing to conceal"
            );
            bake.collect_tribute(0.0, &era);
            arms.push((
                bake.communities[patron].stores,
                bake.communities[sub].population,
            ));
        }
        let (expansive_remitted, expansive_kept) = arms[0];
        let (insular_remitted, insular_kept) = arms[1];

        assert!(
            expansive_remitted > 0.0 && insular_remitted > 0.0,
            "precondition: tribute must flow in BOTH arms — 'less' asserted \
             over two zeros proves nothing: expansive {expansive_remitted}, \
             insular {insular_remitted}"
        );
        assert!(
            insular_remitted < expansive_remitted,
            "an insular people withholds MORE from its patron: insular \
             remitted {insular_remitted}, expansive {expansive_remitted}"
        );
        // The withheld share is not destroyed — it stays with the people who
        // hid it. Without this a concealment implemented as "burn the
        // difference" would pass the comparison above.
        assert!(
            insular_kept > expansive_kept,
            "what is concealed stays with the subordinate: insular kept \
             {insular_kept}, expansive kept {expansive_kept}"
        );
    }

    #[test]
    fn a_non_finite_radius_conceals_nothing() {
        // T4 review, Minor 1. `SocietyVector.in_group_radius` carries its
        // `[0, 1]` contract only as a doc comment — nothing structurally
        // stops a NaN or an infinity from reaching `concealment_of`, even
        // though today's authored values never produce one. Unguarded, a NaN
        // radius would flow through `f64::clamp` (NaN in, NaN out) into
        // `conceal = NaN`, and `collect_tribute`'s cap
        // `assessment.min(surplus * (1.0 - conceal))` would then evaluate to
        // `assessment` — `f64::min` DISCARDS a NaN operand — reaching straight
        // past the growth-increment ceiling into the standing stock, the one
        // thing spec §4.2 forbids. The guard must treat ANY non-finite input
        // (NaN, +inf, -inf) as "conceal nothing", the same failure direction
        // `takes_the_initiative` already takes on a non-finite disposition.
        let goblin_radius: BTreeMap<KindId, f64> =
            [(KindId("goblin"), f64::NAN)].into_iter().collect();
        let kobold_radius: BTreeMap<KindId, f64> =
            [(KindId("kobold"), f64::INFINITY)].into_iter().collect();
        let bugbear_radius: BTreeMap<KindId, f64> = [(KindId("bugbear"), f64::NEG_INFINITY)]
            .into_iter()
            .collect();

        let geo = Geosphere::new(1);
        let graphs = vec![full_land_graph(&geo)];
        let capacity = CellMap::from_fn(&geo, |_| 100.0);
        let river_prox = CellMap::from_fn(&geo, |_| 0.0);
        let refugia = CellMap::from_fn(&geo, |_| false);
        let mut bake = hand_bake(&graphs, &capacity, &river_prox, &refugia, no_disposition());

        bake.in_group_radius = &goblin_radius;
        assert_eq!(
            bake.concealment_of(KindId("goblin")).to_bits(),
            0.0f64.to_bits(),
            "a NaN radius must conceal nothing, not void the growth cap"
        );
        bake.in_group_radius = &kobold_radius;
        assert_eq!(
            bake.concealment_of(KindId("kobold")).to_bits(),
            0.0f64.to_bits(),
            "an infinite radius must conceal nothing either"
        );
        bake.in_group_radius = &bugbear_radius;
        assert_eq!(
            bake.concealment_of(KindId("bugbear")).to_bits(),
            0.0f64.to_bits(),
            "and neither must a negative-infinite one"
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
        bake.collect_tribute(0.0, &era);

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
        bake.collect_tribute(0.0, &era);

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
    fn no_subordinate_is_farmed_below_the_farm_floor_by_tribute() {
        // Spec §8.3 as amendment 3 restates it (spec §4.2b): tribute may bleed
        // a vassal TOWARD `FARM_FLOOR` but never through it.
        //
        // **This replaces `no_subordinate_ends_an_epoch_below_where_it_began_
        // it`, which is no longer true and no longer the invariant.** That
        // guard asserted the milk-don't-kill cap — a remittance bounded by the
        // epoch's growth increment — and the cap is exactly what the amendment
        // removed: a bled vassal is now SUPPOSED to end an epoch below where it
        // began, because that is the only way the tribute loop's own health
        // signal can go negative and the secular cycle can close inside the
        // mechanism. What survives from the old test unchanged is the reason it
        // BOUND: the same hand-driven epoch loop, the same attributability
        // (nothing but tribute moves a population in this world), and the same
        // tightness precondition — the floor must be TOUCHED, not merely
        // respected with slack to spare.
        //
        // The census-level headcount cannot carry this claim
        // (`alive_at_now == records_total`, over the integration fixture):
        // starvation needs `population >= COLLAPSE_PRESSURE * capacity`, and
        // the logistic growth term is bounded BY capacity, so in a quiet world
        // nobody can starve however hard they are farmed — and a subordinate
        // drained to zero does not die, it sits there alive at zero. The claim
        // only has teeth against the per-subordinate population BETWEEN epochs,
        // which `bake()` never exposes.
        //
        // So this drives the bake's own epoch loop by hand — `begin_epoch`,
        // every alive community through `step_community`, then
        // `collect_tribute` — over a value-flat world: uniform capacity, so no
        // cell is ever worth more than its neighbour, every raid the real rule
        // resolves is a subordination, and (asserted below) no war, eviction or
        // famine fires. `grow` and `collect_tribute` are then the ONLY two
        // things that move a population, so a population at the floor is
        // attributable to tribute alone.
        //
        // Note (T4 review, Minor 2): `hand_bake` sets `in_group_radius` to
        // `no_radius()` — concealment 0 throughout — so the shipped
        // configuration (every goblinoid authored below full transparency)
        // is never exercised here. The floor holds for ANY concealment by
        // construction (concealment only ever LOWERS a remittance, and the
        // remittance is bounded by `surplus + bleed` before concealment
        // touches it), but at concealment > 0 a vassal is bled toward the
        // floor more slowly, so the `bled_to_the_floor > 0` tightness
        // precondition below would need a longer run. Left at concealment 0
        // deliberately, not as an oversight.
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

        /// Epochs driven — long enough that relations form early, collect for
        /// most of the run, and bleed a vassal all the way down onto the floor.
        const EPOCHS: usize = 20;
        /// Years per driven epoch (the bake's own default step).
        const EPOCH_YEARS: f64 = 25.0;
        // Slack for the float round-trip of `(p + increment) - remittance`; the
        // floor is otherwise touched exactly, see `bled_to_the_floor`.
        const EPS: f64 = 1.0e-9;
        let mut floor_checks = 0_u32;
        let mut milked_epochs = 0_u32;
        let mut bled_below_where_it_began = 0_u32;
        let mut bled_to_the_floor = 0_u32;
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
            bake.collect_tribute(year, &era);
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
                    ended >= FARM_FLOOR - EPS,
                    "epoch {epoch}: subordinate {sub} was farmed to {ended}, THROUGH the \
                     {FARM_FLOOR} floor (it began the epoch at {start}). A patron may bleed \
                     its vassal toward `FARM_FLOOR` but never past it: tribute alone must \
                     never end a community (spec §4.2b, §8.3)."
                );
                if ended < start - EPS {
                    bled_below_where_it_began += 1;
                }
                if (ended - FARM_FLOOR).abs() < EPS {
                    bled_to_the_floor += 1;
                }
                floor_checks += 1;
            }
        }

        // Anti-vacuity — the same three guards that made the superseded floor
        // bind, plus the one the amendment adds. The floor above is worthless
        // unless relations formed, wealth actually moved along them, the bleed
        // this floor bounds actually engaged, and the bound was TIGHT: a vassal
        // bled to exactly `FARM_FLOOR` is what makes "one head below" a
        // reddening difference rather than slack absorbed by an unspent margin.
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
            bled_below_where_it_began > 0,
            "precondition: the bleed must have ENGAGED — some subordinate must have ended an \
             epoch below where it began it ({floor_checks} readings, none bled). A floor that \
             only ever bounds a population moving upward is not being read at all (spec §4.2b)."
        );
        assert!(
            bled_to_the_floor > 0,
            "precondition: some subordinate must have been bled ONTO the floor, or the floor \
             is never touched and never discriminating ({floor_checks} readings, \
             {bled_below_where_it_began} bled, none tight)"
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
    fn an_over_milked_vassal_shrinks_and_its_patron_eases_off() {
        // Spec §4.2b, the whole point of amendment 3: the "over-extract →
        // collapse → relax" cycle §1 sells must close INSIDE the tribute
        // mechanism, not through the rest of the model.
        //
        // Under the superseded cap it could not. A remittance bounded by the
        // epoch's growth increment guarantees `population_after ≥
        // population_at_epoch_start`, so the health signal §4.3 feeds back on
        // was non-negative from tribute by construction: the demand eased only
        // when war, famine, climate or crowding hurt the vassal. Here the full
        // arc is asserted in order, on a fixture where **nothing but tribute
        // can shrink the vassal**:
        //
        //   1. the vassal ends an epoch BELOW where it began it (the bleed),
        //   2. its patron's demand then FALLS (the signal went negative from
        //      tribute alone, which is the loop closing), and
        //   3. it is never bled through `FARM_FLOOR`.
        //
        // The world is value-flat, so eviction cannot fire; `raided`,
        // `migrated` and `collapsed` are all asserted zero, so a falling
        // population provably has exactly one cause. The relation forms through
        // the real `maybe_raid` path and the pair is driven through the bake's
        // own epoch loop, so the demand that over-reaches is the one the bake
        // sets (`eff_capacity × ASSESS_RATE`), not one a test wrote in.
        let (geo, graphs, capacity, river_prox, refugia, era) = cascade_world(|_| RICH);
        let mut bake = hand_bake(&graphs, &capacity, &river_prox, &refugia, no_disposition());
        let far = geo.neighbors(CellId(0))[0];
        // 40 clears 10 × RAID_MARGIN four times over, and the land is
        // value-flat, so the only prize on offer is the neighbour's product.
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

        /// Epochs driven — long enough for the relation to form, over-reach,
        /// bleed the vassal down and ease off.
        const EPOCHS: usize = 20;
        /// Years per driven epoch (the bake's own default step).
        const EPOCH_YEARS: f64 = 25.0;
        /// Slack for the float round-trip of `(p + increment) - remittance`.
        const EPS: f64 = 1.0e-9;

        // The first epoch in which the bleed engaged, and the arc it produced:
        // (epoch, population at epoch start, population after collection,
        //  demand the collection ran on, demand it left behind).
        let mut first_bleed: Option<(usize, f64, f64, f64, f64)> = None;
        let mut bled = 0_u32;
        let mut eased_after_a_bleed = 0_u32;
        let mut low = bake.communities[sub].population;
        for epoch in 0..EPOCHS {
            let year = epoch as f64 * EPOCH_YEARS;
            bake.begin_epoch();
            let began = bake.communities[sub].population;
            let alive: Vec<usize> = (0..bake.communities.len())
                .filter(|&i| bake.communities[i].alive)
                .collect();
            for idx in alive {
                bake.step_community(idx, &era, year);
            }
            // The standing demand the collection is about to run on, read AFTER
            // formation so the epoch a relation opens in counts too.
            let demanded = bake.tribute.get(&sub).map(|t| (t.patron, t.assessment));
            bake.collect_tribute(year, &era);
            let ended = bake.communities[sub].population;
            low = low.min(ended);
            let Some((patron_before, demand_before)) = demanded else {
                continue; // not yet subordinated
            };
            let (patron_after, demand_after) = bake
                .tribute
                .get(&sub)
                .map(|t| (t.patron, t.assessment))
                .expect("the relation must still stand: nothing closes in this world");
            // The arc is only readable while the SAME patron holds the vassal —
            // a transfer resets the assessment to the new patron's opening
            // demand, which would look like an easing and is not one.
            assert_eq!(
                patron_before, patron_after,
                "epoch {epoch}: the patronage changed hands mid-epoch; a transfer resets the \
                 demand and the easing arc below would be reading the reset, not the feedback"
            );
            assert_eq!(
                patron_after, patron,
                "epoch {epoch}: the vassal must be held by the patron this fixture built"
            );
            if ended < began - EPS {
                bled += 1;
                if demand_after < demand_before {
                    eased_after_a_bleed += 1;
                }
                if first_bleed.is_none() {
                    first_bleed = Some((epoch, began, ended, demand_before, demand_after));
                }
            }
        }

        // Attributability: nothing else in this world moves a population, so a
        // vassal that shrank was shrunk by tribute.
        assert_eq!(
            bake.tally.raided, 0,
            "value-flat world: no eviction, so no war loss can be mistaken for the bleed"
        );
        assert_eq!(
            bake.tally.migrated, 0,
            "no cell turns hostile here: a migration would confound the bleed"
        );
        assert_eq!(
            bake.tally.collapsed, 0,
            "nobody starves here: a famine death would confound the bleed"
        );
        assert!(
            bake.tally.tribute_collected > 0.0,
            "precondition: tribute must actually have flowed ({})",
            bake.tally.tribute_collected
        );

        let (epoch, began, ended, demand_before, demand_after) = first_bleed.expect(
            "the bleed never engaged: no epoch left the vassal below where it began it, so the \
             health signal never went negative from tribute and the cycle does not close inside \
             the mechanism (spec §4.2b)",
        );
        // (1) the bleed.
        assert!(
            ended < began - EPS,
            "epoch {epoch}: the vassal must end BELOW where it began ({ended} vs {began})"
        );
        // (2) …and the demand eased BECAUSE of it. The demand it eased from
        //     must be non-zero, or "the assessment fell" is a statement about a
        //     demand that was already extinguished.
        assert!(
            demand_before > 0.0,
            "epoch {epoch}: precondition — the standing demand must be non-zero before it can \
             meaningfully fall ({demand_before})"
        );
        assert!(
            demand_after < demand_before,
            "epoch {epoch}: the vassal shrank from {began} to {ended} — from TRIBUTE, nothing \
             else moves a population here — so the patron must ease its demand: {demand_after} \
             vs the {demand_before} it collected on. This is the arc amendment 3 exists to \
             close (spec §4.2b)."
        );
        assert_eq!(
            bled, eased_after_a_bleed,
            "every epoch that bled the vassal must have eased the demand ({bled} bled, \
             {eased_after_a_bleed} eased): a bleed that left the demand standing is a signal \
             that did not reach the controller"
        );
        // (3) …and the floor held throughout.
        assert!(
            low >= FARM_FLOOR - EPS,
            "the vassal was bled THROUGH the floor to {low} (floor {FARM_FLOOR}): a patron may \
             bleed toward it, never past it (spec §4.2b, §8.3)"
        );
    }

    #[test]
    fn a_generational_patron_leaves_a_markedly_larger_vassal_than_an_immediate_one() {
        // Spec §4.3a — the first test in this campaign that binds PATRON-SIDE
        // character, and the failure it exists to catch is the one the
        // extraction investigation measured: across 2258 relations over seeds
        // 1..=24, `assessment_at_formation / eff_capacity` took exactly ONE
        // value (0.025). The only per-people input anywhere in the tribute
        // rule was the VASSAL's concealment, so a Sopranos bust-out and a
        // Roman census were not merely the same code path — nothing in the
        // model could tell them apart even in principle.
        //
        // The two arms differ in EXACTLY ONE input: the **patron** people's
        // authored `time_horizon`. Same world, same pair, same populations,
        // same stream, same concealment (none), and every other people pinned
        // at the same horizon in both arms, so a vassal that a kobold daughter
        // ever came to hold is held on identical terms in both. Nothing but
        // the patron's discount rate can explain a difference.
        //
        // The two values are authored ones the shipped roster actually reaches
        // patron-side (bugbear 0.3, kobold 0.8), applied to this fixture's
        // patron people so that the people itself — and therefore its
        // concealment, disposition and tech — is held fixed.
        //
        // The Tolerance (2026-08-04) corrects why these two: the original
        // reason given was that they were the ONLY two reachable, goblin's 0.5
        // being raid-vetoed and hobgoblin sitting at the neutral middle. That
        // premise is gone — `threat_response` is drawn per settlement now, so
        // every people reaches patronhood on some settlements
        // ([`BakeConfig::time_horizon`]) and the reachable set is the whole
        // roster: gnoll 0.2, bugbear 0.3, goblin 0.5, hobgoblin 0.5, human
        // 0.75, kobold 0.8. 0.3 and 0.8 are KEPT unchanged, and are still a
        // good pair: they are far apart, both away from `NEUTRAL_HORIZON`, and
        // both authored rather than invented. They are no longer the extremes
        // — gnoll's 0.2 is now the shortest reachable horizon — but this test
        // asserts a DIRECTION (a generational patron leaves a larger vassal),
        // not a maximal spread, so a wider pair would not make it bind harder.
        //
        // Non-vacuity is asserted, not assumed: BOTH arms must actually have
        // extracted something from this vassal, or "the generational patron
        // left it larger" would pass on a fixture where nobody extracted at
        // all — which is precisely what a rule with no patron-side term looks
        // like when the demand happens never to bind.
        let (geo, graphs, capacity, river_prox, refugia, era) = cascade_world(|_| RICH);
        /// The immediate patron's horizon — bugbear's authored value. The
        /// **second**-shortest reachable, not the shortest: gnoll's 0.2 is
        /// shorter and has been reachable patron-side since The Tolerance made
        /// the raid gate a per-settlement draw (see the block comment above,
        /// and [`BakeConfig::time_horizon`]).
        const IMMEDIATE: f64 = 0.3;
        /// The generational patron's horizon — kobold's authored value, and
        /// still the longest sighted on the roster (human's 0.75 is the next
        /// highest).
        const GENERATIONAL: f64 = 0.8;
        /// Epochs driven: long enough for the relation to form, for the
        /// adaptive demand to climb into contact with the vassal's increment,
        /// and for each arm to settle around its own setpoint.
        const EPOCHS: usize = 40;
        /// Years per driven epoch (the bake's own default step).
        const EPOCH_YEARS: f64 = 25.0;

        // (what this vassal handed over in total, what it was left standing at)
        let mut arms: Vec<(f64, f64)> = Vec::new();
        for horizon in [IMMEDIATE, GENERATIONAL] {
            // Only the patron people's entry differs between the arms; every
            // other people is pinned identically in both.
            let horizons: BTreeMap<KindId, f64> = [
                (KindId("goblin"), horizon),
                (KindId("kobold"), 0.0),
                (KindId("hobgoblin"), 0.0),
                (KindId("bugbear"), 0.0),
            ]
            .into_iter()
            .collect();
            let mut bake = hand_bake(&graphs, &capacity, &river_prox, &refugia, no_disposition());
            bake.time_horizon = &horizons;
            let far = geo.neighbors(CellId(0))[0];
            // 40 clears 10 × RAID_MARGIN four times over, and the land is
            // value-flat, so the only prize on offer is the neighbour's
            // product: the raid subordinates rather than evicts.
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
            let mut taken = 0.0;
            for epoch in 0..EPOCHS {
                let year = epoch as f64 * EPOCH_YEARS;
                bake.begin_epoch();
                let alive: Vec<usize> = (0..bake.communities.len())
                    .filter(|&i| bake.communities[i].alive)
                    .collect();
                for idx in alive {
                    bake.step_community(idx, &era, year);
                }
                let before_collection = bake.communities[sub].population;
                bake.collect_tribute(year, &era);
                // Nothing but `grow` and `collect_tribute` moves a population
                // in this world (asserted below), and `grow` has already run,
                // so the drop across the collection IS this vassal's
                // remittance — attributable without instrumenting the bake.
                taken += (before_collection - bake.communities[sub].population).max(0.0);
            }
            // The reading is only attributable while a patron of the arm's
            // people holds the vassal: a takeover by a people pinned at 0.0 in
            // both arms would erase the contrast rather than fake it, but it
            // would also mean the number is no longer about this parameter.
            let held_by = bake
                .tribute
                .get(&sub)
                .map(|t| bake.records[bake.communities[t.patron].record].core.people)
                .expect("the vassal must still be paying somebody: nothing closes in this world");
            assert_eq!(
                held_by,
                KindId("goblin"),
                "the vassal must still be held by a patron of the people this arm varies"
            );
            assert!(
                bake.communities[patron].alive,
                "the patron this fixture built must still be standing"
            );
            assert_eq!(
                bake.tally.raided, 0,
                "value-flat world: no eviction, so no war loss can be mistaken for extraction"
            );
            assert_eq!(
                bake.tally.migrated, 0,
                "no cell turns hostile here: a migration would confound the reading"
            );
            assert_eq!(
                bake.tally.collapsed, 0,
                "nobody starves here: a famine death would confound the reading"
            );
            arms.push((taken, bake.communities[sub].population));
        }
        let (immediate_took, immediate_left) = arms[0];
        let (generational_took, generational_left) = arms[1];

        assert!(
            immediate_took > 0.0 && generational_took > 0.0,
            "precondition: BOTH patrons must actually have extracted from this vassal — \
             'left it larger' asserted over an arm that took nothing proves nothing: \
             immediate took {immediate_took}, generational took {generational_took}"
        );
        /// How much larger a generational patron's vassal must stand than an
        /// immediate one's for the difference to be a STRATEGY rather than
        /// float noise. The setpoints differ by a factor of 2.5 on this
        /// fixture's land (`FARM_FLOOR + h × (eff/2 − FARM_FLOOR)` at
        /// h = 0.3 vs 0.8), so a rule that reads the horizon at all clears
        /// this comfortably and one that ignores it cannot clear it at all.
        const MARKEDLY: f64 = 1.5;
        assert!(
            generational_left > immediate_left * MARKEDLY,
            "the patron's horizon must set where it steers its vassal (spec §4.3a): the \
             generational patron left {generational_left}, the immediate one {immediate_left} \
             — under {MARKEDLY}× apart, which is what a model with NO patron-side term looks \
             like (the pre-amendment state: one extraction rate for every relation in \
             existence)"
        );
    }

    #[test]
    fn a_generational_patron_declines_a_vassal_too_small_to_farm() {
        // Spec §4.3b, and the measurement that forced it: 45.7% of relations
        // opened on a community sitting at `DAUGHTER_POP`, always below the low
        // root (`crash_basin_fraction() × eff`, ≈ 0.1464), where the opening
        // demand already exceeds everything the vassal can grow. Those
        // relations were doomed at conception — the setpoint decides where a
        // FARMABLE vassal rests and says nothing about one that never was.
        //
        // BOTH ARMS ARE BOUND ON THE SAME FIXTURE, because either alone proves
        // nothing: "the generational patron declined" passes trivially on a
        // fixture where no raid was possible at all, and "the immediate patron
        // took it" says nothing about foresight. Same world, same pair, same
        // populations, same cells, same stream; the ONLY difference between the
        // arms is the patron people's authored `time_horizon`.
        let (geo, graphs, capacity, river_prox, refugia, era) = cascade_world(|_| RICH);
        /// The immediate patron's horizon — bugbear's authored value. The
        /// **second**-shortest reachable, not the shortest: gnoll's 0.2 is
        /// shorter and has been reachable patron-side since The Tolerance made
        /// the raid gate a per-settlement draw (see
        /// [`BakeConfig::time_horizon`]). The arms are unchanged by that —
        /// this test asserts that a generational patron DECLINES a vassal an
        /// immediate one takes, which is a claim about the two arms differing,
        /// not about either being extremal.
        const IMMEDIATE: f64 = 0.3;
        /// The generational patron's horizon — kobold's authored value, and
        /// still the longest sighted on the roster (human's 0.75 is the next
        /// highest).
        const GENERATIONAL: f64 = 0.8;
        /// The patron's population: comfortably over `DAUGHTER_POP ×
        /// RAID_MARGIN`, so dominance is never what decides either arm.
        const PATRON_POP: f64 = 40.0;

        // The fixture is only interesting if the vassal sits in the band the
        // two horizons disagree about — above what an immediate patron
        // requires, below what a generational one does. Both are read off the
        // rule rather than written in, so a change to `ASSESS_RATE`,
        // `GROWTH_RATE` or the horizon values reddens here (as a precondition)
        // instead of quietly making the test vacuous.
        let low_root = crash_basin_fraction() * RICH;
        assert!(
            DAUGHTER_POP < low_root,
            "precondition: a fresh daughter ({DAUGHTER_POP}) must sit BELOW the low root \
             ({low_root}) — that is the measured pathology this rule addresses"
        );
        assert!(
            IMMEDIATE * low_root < DAUGHTER_POP && DAUGHTER_POP < GENERATIONAL * low_root,
            "precondition: this vassal ({DAUGHTER_POP}) must sit between the two arms' \
             thresholds ({} and {}), or the arms are not being asked a question they can \
             answer differently",
            IMMEDIATE * low_root,
            GENERATIONAL * low_root
        );

        // (did a relation form, the subordinate's population after the raid)
        let mut arms: Vec<(bool, f64)> = Vec::new();
        for horizon in [IMMEDIATE, GENERATIONAL] {
            // Only the patron people's entry differs between the arms; every
            // other people is pinned identically in both.
            let horizons: BTreeMap<KindId, f64> = [
                (KindId("goblin"), horizon),
                (KindId("kobold"), 0.0),
                (KindId("hobgoblin"), 0.0),
                (KindId("bugbear"), 0.0),
            ]
            .into_iter()
            .collect();
            let mut bake = hand_bake(&graphs, &capacity, &river_prox, &refugia, no_disposition());
            bake.time_horizon = &horizons;
            let far = geo.neighbors(CellId(0))[0];
            // Value-flat land, so `t_val > raider_val` is false and the only
            // prize on offer is the neighbour's product: a raid here can only
            // ever resolve as a subordination.
            let patron = bake.open(
                KindId("goblin"),
                CellId(0),
                0.0,
                PATRON_POP,
                Founding::Genesis(CellId(0)),
                None,
                0.0,
            );
            let target = bake.open(
                KindId("kobold"),
                far,
                0.0,
                DAUGHTER_POP,
                Founding::Genesis(far),
                None,
                0.0,
            );
            let index_before = bake.node_index.clone();

            bake.maybe_raid(patron, &era, 0.0);

            let formed = bake.tribute.contains_key(&target);
            // A DECLINED subordination is not a raid: no spoil is taken, so no
            // tally moves and nobody is displaced. Asserted on both arms —
            // subordination never evicts, so these hold whichever way the gate
            // went, and a gate that quietly tallied a refusal would show here.
            assert_eq!(
                bake.tally.raided, 0,
                "value-flat land: no eviction is available, so nothing may be counted as one"
            );
            assert_eq!(
                bake.tally.fled, 0,
                "nobody is driven off by a subordination"
            );
            assert_eq!(
                bake.tally.patronage_transfers, 0,
                "the target was paying nobody: no patronage can have changed hands"
            );
            assert_eq!(
                bake.tally.subordinations_formed,
                u64::from(formed),
                "the formation tally must agree with the relation table exactly: a decline \
                 must count nothing, and a take exactly one"
            );
            assert_eq!(
                bake.node_index, index_before,
                "neither outcome moves anybody: the one-alive-per-site index must be untouched"
            );
            assert!(
                bake.communities[patron].alive && bake.communities[target].alive,
                "both communities must survive either outcome"
            );
            arms.push((formed, bake.communities[target].population));
        }
        let (immediate_took, immediate_left) = arms[0];
        let (generational_took, generational_left) = arms[1];

        // The non-vacuity arm, asserted POSITIVELY: this fixture is known to be
        // capable of producing a relation, so "declined" below is a decision
        // and not an absence of opportunity.
        assert!(
            immediate_took,
            "precondition: the short-horizon patron must actually have TAKEN this vassal — \
             without that, 'the generational patron declined' is a statement about a fixture \
             where no subordination was on offer at all"
        );
        assert!(
            !generational_took,
            "a patron with foresight must decline a vassal too small to farm (spec §4.3b): \
             this one sits at {DAUGHTER_POP}, under the {} a horizon of {GENERATIONAL} \
             requires and under the low root {low_root} at which the opening demand stops \
             exceeding everything the vassal can grow — yet the relation formed anyway",
            GENERATIONAL * low_root
        );
        assert_eq!(
            immediate_left.to_bits(),
            generational_left.to_bits(),
            "the gate must decide whether a relation FORMS, never how big the target is: \
             {immediate_left} vs {generational_left}"
        );
    }

    #[test]
    fn a_busy_patron_extracts_harder_from_the_same_vassal_than_a_quiet_one() {
        // Spec §4.3c, the portfolio effect — and the reason it is worth having
        // on top of §4.3a: the per-people axis is COARSE and partly confounded
        // with `sociality`, so a second, independent source of variation earns
        // its place. The subordinate count is a property of the RELATION
        // TABLE, not of a people, so it varies independently of any per-people
        // confound whatever.
        //
        // The Tolerance (2026-08-04) re-derives that coarseness rather than
        // deleting the claim, because the arithmetic behind it moved. It used
        // to read: only THREE values are reachable patron-side (bugbear 0.3 /
        // hobgoblin 0.5 / kobold 0.8, goblin being raid-vetoed), and bugbear is
        // both the short extreme AND the only `Communal` short-horizon people.
        // Both halves are false, and they went false at DIFFERENT times, which
        // is worth separating. The Vacancy's gnoll (horizon 0.2,
        // `threat_response` 0.85) has cleared the raid gate since it was
        // authored, so bugbear stopped being the short extreme one campaign
        // ago, and gnoll is `Hierarchic`, not `Communal`. The Tolerance then
        // falsified the count as well: `threat_response` is drawn per
        // settlement, so every people reaches patronhood on some settlements
        // and the reachable set is the whole roster — gnoll 0.2, bugbear 0.3,
        // goblin 0.5, hobgoblin 0.5, human 0.75, kobold 0.8, i.e. FIVE distinct
        // thresholds (goblin and hobgoblin share 0.5), up from four.
        //
        // The conclusion survives both corrections, for a reason worth stating
        // rather than asserting. Five cuts of one band is still coarse against
        // the continuum the formula could express, so the portfolio effect
        // still earns its place. The `sociality` confound is genuinely WEAKER
        // than the old comment claimed — the short end of the axis is no longer
        // uniquely `Communal` — but it is not gone: the two `Communal` peoples
        // are bugbear (0.3) and kobold (0.8), which is to say the horizon axis
        // still does not vary independently of social form across the roster.
        //
        // **The two arms hold the patron people and its authored horizon
        // FIXED** and differ in exactly one thing: how many OTHER vassals the
        // same patron holds. Anything that varied the horizon here would be
        // re-measuring §4.3a.
        //
        // The measurement is the campaign's own definition of extraction rate —
        // remitted ÷ the vassal's standing population — and NOT total wealth.
        // Those two disagree, which is the whole content of §4.3a: a patron
        // sitting at maximum sustainable yield collects the largest absolute
        // stream there is. "Extracts harder" means it takes a bigger share of a
        // smaller vassal, so the rate is the statistic and the resting
        // population is its shadow; both are asserted.
        let (geo, graphs, capacity, river_prox, refugia, era) = cascade_world(|_| RICH);
        /// The patron people's authored horizon, IDENTICAL on both arms —
        /// kobold's value, the longest sighted the shipped roster reaches, so
        /// the shortening has the widest room to show.
        const HORIZON: f64 = 0.8;
        /// How many further vassals the busy patron holds. Five holdings in
        /// total is inside the range live worlds actually reach
        /// (`max_subordinates` measured 6 at T5d), so this is not an
        /// extrapolation off the end of the model.
        const EXTRAS: usize = 4;
        /// The population every vassal opens at — comfortably above the
        /// quiet patron's setpoint, so BOTH arms bleed on the first collection
        /// and neither reading starts from an empty relation.
        const VASSAL_POP: f64 = 60.0;
        /// The patron's own population. It never raids in this fixture (only
        /// `grow` and `collect_tribute` are driven), so this is only its
        /// standing size.
        const PATRON_POP: f64 = 40.0;
        /// Epochs driven — long enough for each arm to settle at its own
        /// setpoint and for the settled rate, not the opening bleed, to
        /// dominate the reading.
        const EPOCHS: usize = 40;
        /// Years per driven epoch (the bake's own default step).
        const EPOCH_YEARS: f64 = 25.0;

        // Only the patron people's entry carries the horizon; every other
        // people is pinned identically on both arms, so a people change could
        // not explain a difference even if one occurred.
        let horizons: BTreeMap<KindId, f64> = [
            (KindId("goblin"), HORIZON),
            (KindId("kobold"), 0.0),
            (KindId("hobgoblin"), 0.0),
            (KindId("bugbear"), 0.0),
        ]
        .into_iter()
        .collect();

        // (total remitted by the focus vassal, the summed population it was
        //  read against, where it was left standing)
        let mut arms: Vec<(f64, f64, f64)> = Vec::new();
        for extras in [0, EXTRAS] {
            let mut bake = hand_bake(&graphs, &capacity, &river_prox, &refugia, no_disposition());
            bake.time_horizon = &horizons;
            let ring = geo.neighbors(CellId(0));
            assert!(
                ring.len() > EXTRAS,
                "precondition: the patron's cell must have room for {EXTRAS} further vassals \
                 beside the focus one ({} neighbours)",
                ring.len()
            );
            let patron = bake.open(
                KindId("goblin"),
                CellId(0),
                0.0,
                PATRON_POP,
                Founding::Genesis(CellId(0)),
                None,
                0.0,
            );
            // The focus vassal — bit-for-bit the same community on both arms.
            let focus = bake.open(
                KindId("kobold"),
                ring[0],
                0.0,
                VASSAL_POP,
                Founding::Genesis(ring[0]),
                None,
                0.0,
            );
            let mut vassals = vec![focus];
            for site in ring.iter().copied().skip(1).take(extras) {
                vassals.push(bake.open(
                    KindId("kobold"),
                    site,
                    0.0,
                    VASSAL_POP,
                    Founding::Genesis(site),
                    None,
                    0.0,
                ));
            }
            // Relations are seated by hand rather than raided into existence,
            // so the arms differ in the relation TABLE and in nothing else —
            // no raid runs, no tally moves, and the extra vassals cannot reach
            // the focus one through any path but the patron's holdings count.
            // The assessment is deliberately oversized (this file's collection
            // idiom, cf. `tribute_pair`): with the demand never binding, the
            // remittance is exactly what stands above the patron's setpoint, so
            // the reading is about the setpoint and nothing else.
            for &v in &vassals {
                bake.tribute.insert(
                    v,
                    Tribute {
                        patron,
                        assessment: 1.0e9,
                        since: 0.0,
                        last_seen_population: bake.communities[v].population,
                    },
                );
            }

            let mut taken = 0.0;
            let mut population_seen = 0.0;
            for epoch in 0..EPOCHS {
                let year = epoch as f64 * EPOCH_YEARS;
                bake.begin_epoch();
                for &v in &vassals {
                    let pressure = bake.pressure_of(v, &era);
                    bake.grow(v, &era, year, pressure);
                }
                let before = bake.communities[focus].population;
                bake.collect_tribute(year, &era);
                // `grow` has already run and nothing else in this fixture moves
                // a population, so the drop across the collection IS the focus
                // vassal's remittance.
                taken += (before - bake.communities[focus].population).max(0.0);
                population_seen += before;
            }
            assert_eq!(
                bake.tally.raided, 0,
                "no raid is driven here: a war loss must not be mistaken for extraction"
            );
            assert_eq!(
                bake.tally.collapsed, 0,
                "nobody starves here: a famine death would confound the reading"
            );
            assert_eq!(
                bake.tribute.len(),
                vassals.len(),
                "every seated relation must still stand: a dissolution would change the count \
                 mid-run and the arms would no longer differ in one thing"
            );
            arms.push((taken, population_seen, bake.communities[focus].population));
        }
        let (quiet_took, quiet_seen, quiet_left) = arms[0];
        let (busy_took, busy_seen, busy_left) = arms[1];

        assert!(
            quiet_took > 0.0 && busy_took > 0.0,
            "precondition: BOTH patrons must actually have extracted from this vassal — \
             'extracted harder' asserted over two zeros proves nothing: quiet took \
             {quiet_took}, busy took {busy_took}"
        );
        let quiet_rate = quiet_took / quiet_seen;
        let busy_rate = busy_took / busy_seen;
        /// How much higher the busy patron's extraction rate must run for the
        /// difference to be a STRUCTURAL strategy rather than float noise. A
        /// rule that ignores the holdings count makes the two arms bit-identical
        /// (ratio 1.0), so any margin above one reddens it; this one is set well
        /// clear of that while still far under what the shipped
        /// `PORTFOLIO_HALVING` produces at four extra vassals.
        const MARKEDLY: f64 = 1.25;
        assert!(
            busy_rate > quiet_rate * MARKEDLY,
            "a patron holding many vassals must treat each as more expendable (spec §4.3c): \
             holding {EXTRAS} others it extracted at {busy_rate} of standing population, \
             holding none at {quiet_rate} — under {MARKEDLY}× apart, which is what a model \
             whose effective horizon ignores the holdings count looks like"
        );
        assert!(
            busy_left < quiet_left,
            "…and the shadow of that rate is a smaller vassal: the busy patron left \
             {busy_left}, the quiet one {quiet_left}"
        );
    }

    #[test]
    fn a_patrons_effective_horizon_falls_monotonically_and_never_wraps() {
        // The three shape requirements spec §4.3c's rule has to meet, bound
        // rather than trusted — because the obvious formulation fails the
        // second of them. A linear `horizon × (1 − others/K)` is monotone and
        // anchored, and goes NEGATIVE past `K` holdings: a sufficiently
        // successful patron would then steer its vassals to a setpoint below
        // the floor, i.e. the rule would silently become an extermination
        // order at the top of the distribution. Live worlds reach
        // `max_subordinates` 6 today, but nothing bounds that, so the shape —
        // not a clamp bolted on after it — has to be what makes it impossible.
        let geo = Geosphere::new(1);
        let graphs = vec![full_land_graph(&geo)];
        let capacity = CellMap::from_fn(&geo, |_| RICH);
        let river_prox = CellMap::from_fn(&geo, |_| 0.0);
        let refugia = CellMap::from_fn(&geo, |_| false);
        let mut bake = hand_bake(&graphs, &capacity, &river_prox, &refugia, no_disposition());
        /// The patron people's authored horizon under test — kobold's value,
        /// the longest sighted the shipped roster reaches.
        const HORIZON: f64 = 0.8;
        /// How far up the holdings count to walk. Far past anything a live
        /// world has produced (`max_subordinates` 6 at T5d), which is the
        /// point: nothing in the model bounds a patron's holdings, so the rule
        /// must stay sane where the measurements do not reach.
        const ABSURD: usize = 500;
        let horizons: BTreeMap<KindId, f64> = [(KindId("goblin"), HORIZON)].into_iter().collect();
        bake.time_horizon = &horizons;

        // (1) Anchored: holding no OTHER vassal applies the authored horizon
        // exactly, so this rule modulates §4.3a rather than displacing it.
        assert_eq!(
            bake.effective_horizon(KindId("goblin"), 0).to_bits(),
            bake.horizon_of(KindId("goblin")).to_bits(),
            "a patron with a single vassal has no alternative to it and must apply its \
             authored horizon unchanged"
        );

        // (2) Monotone, and (3) bounded into `(0, horizon]` the whole way up.
        let mut previous = bake.effective_horizon(KindId("goblin"), 0);
        for others in 1..=ABSURD {
            let h = bake.effective_horizon(KindId("goblin"), others);
            assert!(
                h < previous,
                "the effective horizon must fall strictly with every further holding: \
                 {others} others gave {h}, {} others gave {previous}",
                others - 1
            );
            assert!(
                h > 0.0 && h <= HORIZON,
                "the effective horizon must stay inside (0, {HORIZON}] at every count — a \
                 patron with {others} vassals read {h}, which is a horizon no setpoint rule \
                 can mean"
            );
            previous = h;
        }

        // …and the setpoint it feeds inherits the bound: even at an absurd
        // holdings count the vassal is steered TOWARD the floor, never through
        // it, which is the failure a sign flip would actually cause.
        let target = bake.target_stock(KindId("goblin"), ABSURD, RICH);
        assert!(
            target >= FARM_FLOOR,
            "a setpoint under the most extreme portfolio the shape admits must still sit at \
             or above the floor: {target} vs {FARM_FLOOR}"
        );
        assert!(
            target < bake.target_stock(KindId("goblin"), 0, RICH),
            "…and below the quiet patron's setpoint, or the rule is doing nothing"
        );
    }

    #[test]
    fn a_vassal_taxed_past_what_it_can_regrow_leaves() {
        // Spec §4.3d, FLIGHT — the first of the two answers the subjugated
        // have beyond concealment. A demand taking a larger share of the
        // community than its own growth law could ever return
        // (`FLIGHT_BURDEN`) is one it will not go on paying: it closes its
        // occupation and takes the road.
        //
        // Three things this test binds that a weaker one would not:
        //   (a) **Non-vacuity.** The relation must have existed and tribute
        //       must actually have flowed, so "it ended" is not trivially
        //       true of a fixture where nobody was ever subordinated.
        //   (b) **It LEFT — it was not driven off.** `fled` means evicted by a
        //       raider and nothing else; a flight tallied there would corrupt
        //       the census the campaign reports. The record must close as an
        //       orderly `Migrated`/`Ended::Nature`, and the people must turn
        //       up alive somewhere else.
        //   (c) **Independence from revolt.** The vassal is pinned well below
        //       its patron's strength, and `vassal_revolts` is asserted zero,
        //       so nothing here can pass on the other mechanism's back.
        let (geo, graphs, capacity, river_prox, refugia, era) = cascade_world(|_| RICH);
        /// The vassal's population: far enough above `VIABLE_MIN /
        /// MIGRATE_SURVIVAL` that the road is survivable, so the departure is
        /// a real choice of this rule and not a husk that could not have gone
        /// anywhere.
        const VASSAL_POP: f64 = 20.0;
        /// The patron's population: over `VASSAL_POP × RAID_MARGIN`, so the
        /// vassal cannot revolt and this fixture measures flight alone.
        const PATRON_POP: f64 = 40.0;
        /// The standing demand. Above `FLIGHT_BURDEN` of what the vassal holds
        /// when it pays, and far below the `bleed` this fixture makes
        /// available (the whole stock above a floor-targeting patron's
        /// setpoint), so the remittance is exactly the demand and the burden
        /// is exactly what the arithmetic says.
        ///
        /// **No `grow` is driven here.** The remittance comes out of the
        /// standing stock, which is a real collection path and keeps the
        /// `DAUGHTER_PROB` draw out of the fixture — a daughter of this
        /// vassal's own lineage would be indistinguishable from the community
        /// the flight founds, and the arrival check below would then be
        /// satisfiable by the wrong event.
        const DEMAND: f64 = 6.0;
        let mut bake = hand_bake(&graphs, &capacity, &river_prox, &refugia, no_disposition());
        let far = geo.neighbors(CellId(0))[0];
        let patron = bake.open(
            KindId("goblin"),
            CellId(0),
            0.0,
            PATRON_POP,
            Founding::Genesis(CellId(0)),
            None,
            0.0,
        );
        let sub = bake.open(
            KindId("kobold"),
            far,
            0.0,
            VASSAL_POP,
            Founding::Genesis(far),
            None,
            0.0,
        );
        let sub_lineage = bake.communities[sub].lineage;
        bake.tribute.insert(
            sub,
            Tribute {
                patron,
                assessment: DEMAND,
                since: 0.0,
                last_seen_population: VASSAL_POP,
            },
        );
        assert!(
            bake.strength(sub) <= bake.strength(patron) * RAID_MARGIN,
            "precondition: the vassal must NOT be able to revolt, or this fixture measures \
             the wrong mechanism"
        );

        bake.begin_epoch();
        let before_collection = bake.communities[sub].population;
        assert!(
            before_collection * MIGRATE_SURVIVAL >= VIABLE_MIN,
            "precondition: the vassal must be big enough to survive the road, or it would \
             endure rather than leave and this test would pass on the wrong branch"
        );
        // The epoch's three tribute steps, in `bake`'s own order. `settle_
        // revolts` is driven too, so `vassal_revolts == 0` below means the
        // revolt rule RAN and declined — not that it was never asked.
        bake.settle_revolts();
        let fleeing = bake.collect_tribute(0.0, &era);
        bake.resolve_flights(fleeing, &era, 0.0);

        // (a) The relation existed and tribute genuinely flowed out of it.
        assert_eq!(
            bake.tally.tribute_collection_events, 1,
            "precondition: the relation must have been collected on, or 'it ended' is vacuous"
        );
        assert!(
            bake.tally.tribute_collected > before_collection * FLIGHT_BURDEN,
            "precondition: the remittance ({}) must exceed {FLIGHT_BURDEN} of the {} the vassal \
             was holding when it paid, or the threshold was never crossed",
            bake.tally.tribute_collected,
            before_collection
        );

        // (b) It left, and left as a leaving.
        assert_eq!(
            bake.tally.vassal_flights, 1,
            "the vassal must have taken flight (spec §4.3d)"
        );
        assert_eq!(
            bake.tally.migrated, 1,
            "a flight is an orderly, self-directed move — the same tally a climate migration takes"
        );
        assert_eq!(
            bake.tally.fled, 0,
            "a flight must NEVER be tallied as `fled`: that counter means driven off by a raider"
        );
        assert_eq!(
            bake.tally.raided, 0,
            "nobody raided anybody here — the vassal walked"
        );
        assert!(
            !bake.communities[sub].alive,
            "the fleeing community's occupation must be closed"
        );
        let rec = &bake.records[bake.communities[sub].record];
        assert_eq!(
            (rec.core.cause, rec.ended_by),
            (Some(CauseOfEnd::Migrated), Ended::Nature),
            "it left of its own accord: an orderly migration, ended by nobody"
        );
        assert!(
            !bake.tribute.contains_key(&sub),
            "…and the closed community is party to nothing"
        );
        let arrived = bake
            .communities
            .iter()
            .enumerate()
            .find(|(i, c)| *i != sub && c.alive && c.lineage == sub_lineage)
            .map(|(i, _)| i)
            .expect("the fleeing people must be standing somewhere else — it left, it did not die");
        assert_ne!(
            bake.communities[arrived].site, far,
            "…on a different cell from the one it abandoned"
        );
        assert_eq!(
            bake.tribute.get(&arrived).map(|t| t.patron),
            None,
            "the leaver arrives FREE: spec §4.3e's continuity is role-asymmetric, so a \
             relocating community drops what it owed. Obligation that followed the runaway \
             would make flight a change of address rather than the escape §4.3d calls \
             'relocating rather than continue in that condition'"
        );
        assert!(
            bake.tribute.is_empty(),
            "…and the relation is gone from the table entirely, not merely re-keyed onto \
             some other index"
        );
        assert!(
            bake.communities[patron].alive
                && bake.communities[patron].site == CellId(0)
                && bake.communities[patron].stores > 0.0,
            "the patron keeps its cell and what it already collected: it loses the stream, \
             not the hoard — the flight is not a raid on it"
        );

        // (c) Nothing here was a revolt.
        assert_eq!(
            bake.tally.vassal_revolts, 0,
            "independence: this fixture must exercise flight alone"
        );
    }

    #[test]
    fn a_flight_with_nowhere_to_go_is_a_death_not_a_departure() {
        // Final review, Important 2. `vassal_flights` is documented and read as
        // a **strict subset of `migrated`** — "this counts departures, never
        // deaths". Deciding to leave and succeeding in leaving are two events,
        // and the tally belongs on the second: `take_flight` closes the
        // occupation and THEN calls `relocate`, which can return `Lost`.
        // Counted before that call, a vassal that walked off and died on the
        // road was counted as a flight while being tallied `collapsed`, so
        // `vassal_flights` could exceed `migrated` and the subset claim was
        // false.
        //
        // The shape that reaches it: a vassal with a real reason to leave and
        // **nowhere admissible to go**. Only two cells in this world are
        // habitable at all — the patron's and the vassal's — so once the
        // vassal's own cell is closed behind it (`from` is never offered to
        // `best_home`) the whole map is either worthless or held by a lord it
        // cannot beat. The road ends nowhere.
        //
        // Mutation-verified in both directions: with the tally restored to its
        // pre-fix position (before `relocate`) clause (b) reddens at
        // `vassal_flights` 1 vs 0.
        let geo = Geosphere::new(1);
        let graphs = vec![full_land_graph(&geo)];
        let capacity = CellMap::from_fn(&geo, |_| RICH);
        let river_prox = CellMap::from_fn(&geo, |_| 0.0);
        let refugia = CellMap::from_fn(&geo, |_| false);
        let far = geo.neighbors(CellId(0))[0];
        // The whole world is dead ground except the two cells this fixture
        // occupies. `best_home` skips a cell whose habitability factor is zero,
        // so nothing outside these two is ever a candidate.
        let era = {
            use hornvale_kernel::ReferenceElevation;
            EraClimate {
                day: 0.0,
                ice: CellMap::from_fn(&geo, |_| false),
                habitable: CellMap::from_fn(&geo, |c| c == CellId(0) || c == far),
                sea_level: ReferenceElevation::new(0.0).unwrap(),
                ice_fraction: 0.0,
            }
        };
        /// The vassal's people — big enough that the road is survivable, so the
        /// departure is a real choice and not the `arriving < VIABLE_MIN` guard
        /// declining it (which would never reach the tally at all, and so would
        /// test nothing).
        const VASSAL_POP: f64 = 20.0;
        /// The patron's people — over `VASSAL_POP × RAID_MARGIN`, so the vassal
        /// can neither revolt nor take the one other habitable cell in the
        /// world by force.
        const PATRON_POP: f64 = 40.0;
        /// The standing demand: above `FLIGHT_BURDEN` of what the vassal holds
        /// when it pays, exactly as in `a_vassal_taxed_past_what_it_can_regrow_
        /// leaves`. The two fixtures differ in the era mask and in nothing
        /// else, so the difference in outcome is the road and not the demand.
        const DEMAND: f64 = 6.0;
        let mut bake = hand_bake(&graphs, &capacity, &river_prox, &refugia, no_disposition());
        let patron = bake.open(
            KindId("goblin"),
            CellId(0),
            0.0,
            PATRON_POP,
            Founding::Genesis(CellId(0)),
            None,
            0.0,
        );
        let sub = bake.open(
            KindId("kobold"),
            far,
            0.0,
            VASSAL_POP,
            Founding::Genesis(far),
            None,
            0.0,
        );
        let sub_lineage = bake.communities[sub].lineage;
        bake.tribute.insert(
            sub,
            Tribute {
                patron,
                assessment: DEMAND,
                since: 0.0,
                last_seen_population: VASSAL_POP,
            },
        );

        bake.begin_epoch();
        let before_collection = bake.communities[sub].population;
        assert!(
            before_collection * MIGRATE_SURVIVAL >= VIABLE_MIN,
            "precondition: the vassal must be big enough to survive the road, or it would \
             endure rather than leave and this test would pass on the wrong branch"
        );
        bake.settle_revolts();
        let fleeing = bake.collect_tribute(0.0, &era);
        assert_eq!(
            fleeing.len(),
            1,
            "precondition: the vassal must have DECIDED to leave, or the tally under test \
             is never reached: {fleeing:?}"
        );
        bake.resolve_flights(fleeing, &era, 0.0);

        // (a) It really did leave, and it really did die: the road, not the
        //     guard that declines it.
        assert!(
            !bake.communities[sub].alive,
            "the vassal closed its occupation and took the road"
        );
        assert!(
            !bake
                .communities
                .iter()
                .enumerate()
                .any(|(i, c)| i != sub && c.alive && c.lineage == sub_lineage),
            "…and nothing of its line is standing anywhere: the relocation must have been \
             LOST, or this fixture is measuring a successful flight"
        );

        // (b) THE FINDING. A death is not a departure.
        assert_eq!(
            bake.tally.vassal_flights, 0,
            "a flight that found no home must not be tallied as a flight: `vassal_flights` \
             says it counts departures and is read as a subset of `migrated`"
        );
        assert_eq!(
            bake.tally.migrated, 0,
            "…and nothing migrated, because nothing arrived anywhere"
        );
        assert_eq!(
            bake.tally.collapsed, 1,
            "…it is a death, and a community may not vanish from the world uncounted"
        );
        // The subset claim itself, stated as the invariant rather than as two
        // numbers that happen to agree — this is the line that would fail on
        // any future fixture where a flight can fail.
        assert!(
            bake.tally.vassal_flights <= bake.tally.migrated,
            "`vassal_flights` ({}) must never exceed `migrated` ({}): it is documented as a \
             strict subset of it",
            bake.tally.vassal_flights,
            bake.tally.migrated
        );

        // (c) Nothing here was a raid or a revolt: the fixture measures the
        //     failed road alone.
        assert_eq!(
            (
                bake.tally.raided,
                bake.tally.fled,
                bake.tally.vassal_revolts
            ),
            (0, 0, 0),
            "independence: nobody fought anybody in this world"
        );
    }

    #[test]
    fn a_vassal_that_outgrows_its_patron_throws_off_the_relation() {
        // Spec §4.3d, REVOLT — and the campaign's first mechanism by which
        // accumulated structure can FAIL rather than merely accumulate. When
        // `strength(vassal) > strength(patron) × RAID_MARGIN` the relation
        // dissolves, and the vassal is genuinely free afterward.
        //
        // "The table shrank" is satisfiable by a bug that drops relations
        // wholesale, so freedom is asserted from both ends: the entry is gone,
        // AND the patron collects nothing further from a vassal that is still
        // alive, still on its cell, and still growing. The epoch it revolts in
        // it is not milked either — spec §4.3d's revolt is tested BEFORE
        // collection, so a community strong enough to refuse does refuse.
        //
        // Independence from flight is asserted throughout: the demand is small
        // enough that no burden ever crosses `FLIGHT_BURDEN`, and
        // `vassal_flights` must stay zero.
        let (geo, graphs, capacity, river_prox, refugia, era) = cascade_world(|_| RICH);
        /// The vassal's population — under `PATRON_POP / RAID_MARGIN`, so the
        /// relation opens perfectly legitimate.
        const VASSAL_POP: f64 = 20.0;
        /// The patron's population while it still holds the whip hand.
        const PATRON_POP: f64 = 40.0;
        /// What the patron is left with after the world breaks it (a war, a
        /// famine, a cell the ice took) — chosen so the vassal clears
        /// `RAID_MARGIN` over it and not by an inch: nothing in this file
        /// moves it there, the test does, because *why* the balance swung is
        /// not what this rule reads.
        const BROKEN_PATRON_POP: f64 = 5.0;
        /// A modest standing demand: well under `FLIGHT_BURDEN` of the
        /// vassal's population, so flight can never fire here.
        const DEMAND: f64 = 1.0;
        /// Years per driven epoch (the bake's own default step).
        const EPOCH_YEARS: f64 = 25.0;
        let mut bake = hand_bake(&graphs, &capacity, &river_prox, &refugia, no_disposition());
        let far = geo.neighbors(CellId(0))[0];
        let patron = bake.open(
            KindId("goblin"),
            CellId(0),
            0.0,
            PATRON_POP,
            Founding::Genesis(CellId(0)),
            None,
            0.0,
        );
        let sub = bake.open(
            KindId("kobold"),
            far,
            0.0,
            VASSAL_POP,
            Founding::Genesis(far),
            None,
            0.0,
        );
        bake.tribute.insert(
            sub,
            Tribute {
                patron,
                assessment: DEMAND,
                since: 0.0,
                last_seen_population: VASSAL_POP,
            },
        );

        // Epoch 1 — the relation is real and tribute is flowing out of it.
        // The remittance is paid out of the standing stock (a floor-targeting
        // patron's whole `bleed`), so no `grow` is driven and the fixture
        // consumes no `DAUGHTER_PROB` draw.
        bake.begin_epoch();
        let stores_before = bake.communities[patron].stores;
        bake.settle_revolts();
        let fleeing = bake.collect_tribute(0.0, &era);
        bake.resolve_flights(fleeing, &era, 0.0);
        assert!(
            bake.communities[patron].stores > stores_before,
            "precondition: the patron must actually have been collecting — 'the relation ended' \
             proves nothing about a relation that never paid"
        );
        assert!(
            bake.tribute.contains_key(&sub),
            "precondition: and the relation must still be standing going into the swing"
        );
        assert_eq!(
            (bake.tally.vassal_revolts, bake.tally.vassal_flights),
            (0, 0),
            "precondition: neither mechanism has fired yet"
        );

        // The balance swings — the patron is broken by something this rule
        // does not care about, and the vassal now out-muscles it.
        bake.communities[patron].population = BROKEN_PATRON_POP;
        assert!(
            bake.strength(sub) > bake.strength(patron) * RAID_MARGIN,
            "precondition: the vassal must now clear the dominance margin over its patron"
        );
        let patron_stores = bake.communities[patron].stores;
        let vassal_population = bake.communities[sub].population;
        let events_before = bake.tally.tribute_collection_events;
        bake.begin_epoch();
        bake.settle_revolts();
        let fleeing = bake.collect_tribute(EPOCH_YEARS, &era);
        bake.resolve_flights(fleeing, &era, EPOCH_YEARS);

        assert_eq!(
            bake.tally.vassal_revolts, 1,
            "the relation must dissolve when the vassal out-muscles its patron (spec §4.3d)"
        );
        assert!(
            !bake.tribute.contains_key(&sub),
            "…and the entry must be gone from the relation table"
        );
        assert_eq!(
            bake.tally.tribute_collection_events, events_before,
            "a vassal that revolts is not milked on the way out: revolt is settled BEFORE \
             collection, so no remittance is credited that nobody could have compelled"
        );
        assert_eq!(
            bake.communities[patron].stores.to_bits(),
            patron_stores.to_bits(),
            "…so the patron's hoard does not move in the epoch it loses the relation"
        );
        assert_eq!(
            bake.communities[sub].population.to_bits(),
            vassal_population.to_bits(),
            "…and nothing was taken out of the vassal either"
        );
        assert!(
            bake.communities[sub].alive && bake.communities[sub].site == far,
            "the vassal is FREE, not dead: it keeps its people and its cell (a revolt is not \
             an eviction)"
        );
        assert!(
            bake.communities[patron].alive,
            "…and its former patron is still standing — one relation ended, not a network \
             (spec §9's collapse-release stays deferred)"
        );
        assert_eq!(
            bake.tally.vassal_flights, 0,
            "independence: this fixture must exercise revolt alone"
        );

        // …and it STAYS free. Restore the patron to its full strength and run
        // another epoch: it collects nothing, because the relation is gone.
        // Without this, "the table shrank" would be satisfiable by a bug that
        // drops entries while the collection carries on regardless.
        bake.communities[patron].population = PATRON_POP;
        let patron_stores = bake.communities[patron].stores;
        let events_before = bake.tally.tribute_collection_events;
        bake.begin_epoch();
        assert!(
            bake.communities[sub].population
                > bake.target_stock(KindId("goblin"), 0, RICH) + DEMAND,
            "precondition: the freed vassal still stands well above what its former patron \
             steers a vassal to, so a live relation would certainly have collected the whole \
             demand from it — 'nothing was collected' is therefore about the relation being \
             gone, not about there being nothing to take"
        );
        bake.settle_revolts();
        let fleeing = bake.collect_tribute(2.0 * EPOCH_YEARS, &era);
        bake.resolve_flights(fleeing, &era, 2.0 * EPOCH_YEARS);
        assert_eq!(
            bake.tally.tribute_collection_events, events_before,
            "the patron must stop collecting from a vassal that has thrown it off"
        );
        assert_eq!(
            bake.communities[patron].stores.to_bits(),
            patron_stores.to_bits(),
            "…and its stores must not grow from it again"
        );
        assert_eq!(
            bake.tally.vassal_revolts, 1,
            "one relation, one revolt: a dissolved relation cannot revolt twice"
        );
    }

    #[test]
    fn a_patron_driven_off_its_land_keeps_its_vassals_and_arrives_weakened() {
        // Spec §4.3e, the WOUNDED PATRON — the state this model had no room
        // for, and the reason §4.3d's revolt never fired. Measured before this
        // rule: across thirty worlds the largest strength(vassal) /
        // strength(patron) any relation reaches is 1.029 against a threshold
        // of 1.5, because every path that damages a patron KILLS it. A patron
        // that loses a raid closes its record, and closure dissolves its whole
        // portfolio (spec §4.4). Healthy or dead, never wounded.
        //
        // **Both halves are asserted, and neither alone is the wounded state.**
        // A patron that keeps its vassals without losing strength has not been
        // hurt at all; one that loses strength without keeping its vassals is
        // precisely the shipped behaviour this rule replaces. So the test binds
        // the relation arriving at the NEW seat, *and* the new seat being
        // weaker than the old — and then, because that conjunction is supposed
        // to be causal rather than decorative, that the same vassal which could
        // not revolt before the raid revolts after it.
        //
        // Arithmetic, all Neolithic (weight 1.0) in year 0:
        //   patron   40 pop + 20 stores          → strength 40 + 0.5×20 = 50
        //   attacker 100 pop on POORER land      → 100 > 50 × RAID_MARGIN = 75, and
        //                                          the patron's cell is worth more,
        //                                          so the raid EVICTS
        //   patron after the war                 → 40 × (1 − WAR_LOSS) = 28, and the
        //                                          hoard dies with the old community
        //   vassal   43 pop, no stores           → 43 ≤ 50 × 1.5 before (no revolt)
        //                                          43 >  28 × 1.5 = 42 after (revolt)
        // The vassal is also, at 43, exactly out of the wounded patron's reach
        // on the road (28 ≤ 43 × RAID_MARGIN), so the roll-downhill cannot
        // resolve this test by having the loser eat its own vassal.
        let (geo, graphs, capacity, river_prox, refugia, era) = {
            let geo = Geosphere::new(1);
            let ring = geo.neighbors(CellId(0));
            let (seat, vassal_cell) = (ring[0], ring[1]);
            cascade_world(move |c| match c {
                CellId(0) => 200.0,      // the patron's prize land — the reason it is raided
                c if c == seat => 150.0, // the attacker's poorer holding, vacated by its win
                c if c == vassal_cell => 60.0, // enough that the vassal is no husk
                _ => POOR,
            })
        };
        /// The patron's people before the war.
        const PATRON_POP: f64 = 40.0;
        /// The patron's hoard: strength its land does not feed, and the thing
        /// that makes the gap to a vassal monotone (spec §4.2a). It is lost
        /// with the old community, which is most of the wound.
        const HOARD: f64 = 20.0;
        /// The attacker: over `PATRON_POP` + `HOARD × STORE_WEIGHT` times
        /// `RAID_MARGIN`, so the raid is certain.
        const ATTACKER_POP: f64 = 100.0;
        /// The vassal: under the patron's dominance margin while the patron is
        /// whole, over it once the patron has been beaten. That straddle is the
        /// mechanism, so it is the one number this fixture tunes.
        const VASSAL_POP: f64 = 43.0;
        let mut bake = hand_bake(&graphs, &capacity, &river_prox, &refugia, no_disposition());
        let ring = geo.neighbors(CellId(0));
        let (seat, vassal_cell) = (ring[0], ring[1]);
        let patron = bake.open(
            KindId("goblin"),
            CellId(0),
            0.0,
            PATRON_POP,
            Founding::Genesis(CellId(0)),
            None,
            0.0,
        );
        bake.communities[patron].stores = HOARD;
        let patron_lineage = bake.communities[patron].lineage;
        let vassal = bake.open(
            KindId("kobold"),
            vassal_cell,
            0.0,
            VASSAL_POP,
            Founding::Genesis(vassal_cell),
            None,
            0.0,
        );
        let attacker = bake.open(
            KindId("hobgoblin"),
            seat,
            0.0,
            ATTACKER_POP,
            Founding::Genesis(seat),
            None,
            0.0,
        );
        let attacker_id = bake.communities[attacker].id;
        let terms = Tribute {
            patron,
            assessment: 1.5,
            since: 0.0,
            last_seen_population: VASSAL_POP,
        };
        bake.tribute.insert(vassal, terms);

        // Preconditions. The vassal cannot throw off a WHOLE patron — asserted
        // by driving the revolt rule and watching it decline, so the zero below
        // means the rule ran, not that it was never asked.
        let strength_before = bake.strength(patron);
        bake.settle_revolts();
        assert_eq!(
            bake.tally.vassal_revolts, 0,
            "precondition: an unharmed patron must be unthrowable, or the swing this test \
             measures is not what frees the vassal"
        );
        assert!(
            bake.strength(vassal) <= strength_before * RAID_MARGIN,
            "precondition: {} vs {strength_before} × {RAID_MARGIN}",
            bake.strength(vassal)
        );

        // The raid. The attacker covets the patron's better land, so this is
        // the eviction branch: the patron is driven off and rolls downhill.
        //
        // **Driven at year 200, not at year 0**, so that the carried relation's
        // `since` (0.0, above) and the reseat year are DIFFERENT numbers and
        // the date assertion below can tell them apart. 200 sits inside the
        // same `tech_for` horizon as 0 (Neolithic runs to 400), so every
        // strength this fixture tunes reads exactly as it did — the fixture's
        // arithmetic is untouched, only its clock moved.
        const RESEAT_YEAR: f64 = 200.0;
        bake.maybe_raid(attacker, &era, RESEAT_YEAR);
        assert_eq!(
            bake.tally.raided, 1,
            "precondition: the raid must have happened"
        );
        assert!(
            !bake.communities[patron].alive,
            "precondition: the patron must have LOST its cell — this test is about what \
             survives being beaten, not about being left alone"
        );
        let rec = &bake.records[bake.communities[patron].record];
        assert_eq!(
            (rec.core.cause, rec.ended_by),
            (Some(CauseOfEnd::Fled), Ended::By(attacker_id)),
            "precondition: driven off by the attacker, not a self-directed move"
        );

        // (a) It is somewhere else, alive.
        let reseated = bake
            .communities
            .iter()
            .enumerate()
            .find(|(i, c)| *i != patron && c.alive && c.lineage == patron_lineage)
            .map(|(i, _)| i)
            .expect("the beaten patron must be standing somewhere else — it fled, it did not die");
        assert_ne!(
            bake.communities[reseated].site,
            CellId(0),
            "…on a different cell from the one it lost"
        );

        // (b) It still holds its vassal, and it is the SAME relation — not one
        //     re-formed by some other rule, which would be a different finding
        //     entirely.
        let standing = bake
            .tribute
            .get(&vassal)
            .copied()
            .expect("the obligation must have travelled with the lord (spec §4.3e)");
        assert_eq!(
            standing.patron, reseated,
            "the relation must name the NEW community, not the closed one — a dangling \
             index here is the silent corruption spec §4.4 forbids"
        );
        assert_eq!(
            (
                standing.assessment.to_bits(),
                standing.last_seen_population.to_bits()
            ),
            (
                terms.assessment.to_bits(),
                terms.last_seen_population.to_bits()
            ),
            "…on exactly the terms it stood on: a carried relation keeps its history, so \
             the patron's learned demand is not reset by being beaten"
        );
        // …with ONE exception, and it is the whole of what a reseat re-stamps.
        // The obligation continues; THIS LORD'S TENURE does not — `reseated` is
        // a community minted at `RESEAT_YEAR`, and `since` is the day the
        // emitted `pays-tribute-to` fact is stamped with. Carried forward it
        // would date the fact 200 years before the entity it names existed,
        // which is what 22 of seed 42's 164 tribute facts did (final review,
        // Important 1). Asserted against the reseat year rather than merely
        // "not the old one", so a rule that reset it to some other convenient
        // day fails here too.
        assert_eq!(
            standing.since.to_bits(),
            RESEAT_YEAR.to_bits(),
            "the carried relation's `since` must be re-stamped to the day the new lord was \
             seated ({RESEAT_YEAR}), not carried from the old one ({}): no fact may be dated \
             before either entity it names was founded",
            terms.since
        );
        assert_eq!(
            bake.records[bake.communities[reseated].record]
                .core
                .founded
                .to_bits(),
            standing.since.to_bits(),
            "…which is exactly the reseated lord's own founding day — the anchor the \
             invariant is stated against, read off the record rather than assumed"
        );
        assert_eq!(
            (
                bake.tally.subordinations_formed,
                bake.tally.patronage_transfers
            ),
            (0, 0),
            "nothing was subjugated or transferred here: the relation was CARRIED"
        );
        assert!(
            bake.communities[vassal].alive
                && bake.communities[vassal].site == vassal_cell
                && bake.node_index.get(&vassal_cell) == Some(&vassal),
            "the vassal itself never moved — only its lord did"
        );

        // (c) …and it arrived weaker. Both the war and the lost hoard.
        let strength_after = bake.strength(reseated);
        assert_eq!(
            bake.communities[reseated].stores.to_bits(),
            0.0f64.to_bits(),
            "the hoard does NOT travel: stores die with the community that held them \
             (spec §4.2a), and that is most of what 'wounded' means here"
        );
        assert_eq!(
            strength_after.to_bits(),
            (PATRON_POP * (1.0 - WAR_LOSS)).to_bits(),
            "the war took its share of the people, and nothing replaced the hoard"
        );
        assert!(
            strength_after < strength_before,
            "a patron that kept its vassals AND its strength has not been wounded: \
             {strength_after} vs {strength_before}"
        );
        assert!(
            bake.strength(vassal) / strength_after > bake.strength(vassal) / strength_before,
            "the imbalance must have SWUNG toward the vassal — that swing is the whole \
             of spec §4.3e's claim"
        );

        // (d) The payoff, and the reason this is not merely a realism fix: the
        //     vassal that could not revolt against a whole patron revolts
        //     against a beaten one.
        bake.settle_revolts();
        assert_eq!(
            bake.tally.vassal_revolts, 1,
            "the wounded state must be REACHABLE by revolt, or §4.3d stays inert for the \
             same structural reason it always was"
        );
        assert!(
            !bake.tribute.contains_key(&vassal),
            "…and the vassal is free of it"
        );
    }

    #[test]
    fn a_lords_claim_travels_and_a_runaways_obligation_does_not() {
        // Spec §4.3e, THE ASYMMETRY — and the reason it is one test and not
        // two. Relocation preserves a community's relations **as patron** and
        // dissolves them **as subordinate**, so the direction of a relation
        // decides whether moving carries it. Either arm on its own is
        // satisfiable by a rule that does not distinguish the roles at all: a
        // role-blind "relocation preserves everything" passes the patron arm,
        // and the pre-continuity "closing dissolves everything" passes the
        // runaway arm. Only the CONJUNCTION, on one fixture, one table, one
        // pass, binds the asymmetry — and the loyal vassal that stays put is
        // the control that says the runaway's relation was dropped because it
        // LEFT, not because the flight pass dissolves relations wholesale.
        //
        // The two arms run in sequence on the same three communities:
        //
        //   arm A  an attacker covets the lord's better land and evicts it.
        //          The lord reseats elsewhere STILL HOLDING both vassals —
        //          the claim travels.
        //   arm B  the reseated lord's standing demand on one vassal crosses
        //          `FLIGHT_BURDEN`; that vassal takes the road and arrives
        //          OWING NOBODY, while the other goes on paying — the
        //          obligation does not travel.
        //
        // Arithmetic, all Neolithic (weight 1.0) in year 0:
        //   lord     40 pop + 20 stores       → strength 40 + 0.5×20 = 50
        //   attacker 100 pop on POORER land   → 100 > 50 × RAID_MARGIN = 75, and
        //                                       the lord's cell is worth more, so
        //                                       the raid EVICTS
        //   lord after the war                → 40 × (1 − WAR_LOSS) = 28, hoard lost
        //   each vassal 20 pop, no stores     → 28 ≤ 20 × RAID_MARGIN = 30, so the
        //                                       beaten lord cannot eat a vassal on
        //                                       the road; and 20 ≤ 28 × 1.5, so
        //                                       neither can revolt against it
        //   runaway's demand 6 on 20 pop      → 0.3 > FLIGHT_BURDEN = 0.2  (flees)
        //   stayer's  demand 1 on 20 pop      → 0.05 < FLIGHT_BURDEN       (stays)
        let (geo, graphs, capacity, river_prox, refugia, era) = {
            let geo = Geosphere::new(1);
            let ring = geo.neighbors(CellId(0));
            let (seat, stayer_cell, runaway_cell) = (ring[0], ring[1], ring[2]);
            cascade_world(move |c| match c {
                CellId(0) => 200.0,            // the lord's prize land
                c if c == seat => 150.0,       // the attacker's poorer holding
                c if c == stayer_cell => 60.0, // enough that neither vassal is a husk
                c if c == runaway_cell => 60.0,
                _ => POOR,
            })
        };
        /// The lord's people before the war.
        const LORD_POP: f64 = 40.0;
        /// The lord's hoard — lost with the old community, which is most of the
        /// wound (spec §4.2a).
        const HOARD: f64 = 20.0;
        /// The attacker: over `LORD_POP` + `HOARD × STORE_WEIGHT` times
        /// `RAID_MARGIN`, so the raid is certain.
        const ATTACKER_POP: f64 = 100.0;
        /// Each vassal's people. Small enough not to revolt against the beaten
        /// lord, large enough that the beaten lord cannot take its cell on the
        /// road and that the road is survivable once the demand is paid.
        const VASSAL_POP: f64 = 20.0;
        /// The demand that drives the runaway out: above `FLIGHT_BURDEN` of
        /// what it holds when it pays.
        const HEAVY_DEMAND: f64 = 6.0;
        /// The demand the stayer goes on paying: well below `FLIGHT_BURDEN`.
        const LIGHT_DEMAND: f64 = 1.0;
        let mut bake = hand_bake(&graphs, &capacity, &river_prox, &refugia, no_disposition());
        let ring = geo.neighbors(CellId(0));
        let (seat, stayer_cell, runaway_cell) = (ring[0], ring[1], ring[2]);
        let lord = bake.open(
            KindId("goblin"),
            CellId(0),
            0.0,
            LORD_POP,
            Founding::Genesis(CellId(0)),
            None,
            0.0,
        );
        bake.communities[lord].stores = HOARD;
        let lord_lineage = bake.communities[lord].lineage;
        let stayer = bake.open(
            KindId("kobold"),
            stayer_cell,
            0.0,
            VASSAL_POP,
            Founding::Genesis(stayer_cell),
            None,
            0.0,
        );
        let runaway = bake.open(
            KindId("bugbear"),
            runaway_cell,
            0.0,
            VASSAL_POP,
            Founding::Genesis(runaway_cell),
            None,
            0.0,
        );
        let runaway_lineage = bake.communities[runaway].lineage;
        let attacker = bake.open(
            KindId("hobgoblin"),
            seat,
            0.0,
            ATTACKER_POP,
            Founding::Genesis(seat),
            None,
            0.0,
        );
        let light = Tribute {
            patron: lord,
            assessment: LIGHT_DEMAND,
            since: 0.0,
            last_seen_population: VASSAL_POP,
        };
        let heavy = Tribute {
            patron: lord,
            assessment: HEAVY_DEMAND,
            since: 0.0,
            last_seen_population: VASSAL_POP,
        };
        bake.tribute.insert(stayer, light);
        bake.tribute.insert(runaway, heavy);

        // ─── Arm A: the claim travels ────────────────────────────────────────
        //
        // **The whole test runs at year 200, not year 0**, so the carried
        // relations' `since` (0.0, above) and the reseat year are different
        // numbers and the date assertion below can tell them apart. 200 sits
        // inside the same `tech_for` horizon as 0 (Neolithic runs to 400), so
        // every strength and every burden this fixture tunes reads exactly as
        // it did.
        const RESEAT_YEAR: f64 = 200.0;
        bake.maybe_raid(attacker, &era, RESEAT_YEAR);
        assert_eq!(
            bake.tally.raided, 1,
            "precondition: the raid must have happened"
        );
        assert!(
            !bake.communities[lord].alive,
            "precondition: the lord must have LOST its cell — this arm is about what \
             survives being beaten"
        );
        let reseated = bake
            .communities
            .iter()
            .enumerate()
            .find(|(i, c)| *i != lord && c.alive && c.lineage == lord_lineage)
            .map(|(i, _)| i)
            .expect("the beaten lord must be standing somewhere else — it fled, it did not die");
        for (who, terms, label) in [(stayer, light, "stayer"), (runaway, heavy, "runaway")] {
            let standing = bake.tribute.get(&who).copied().unwrap_or_else(|| {
                panic!(
                    "the {label}'s obligation must have travelled with \
                     the lord: a relocating community keeps its relations AS PATRON \
                     (spec §4.3e)"
                )
            });
            assert_eq!(
                standing.patron, reseated,
                "the {label}'s relation must name the NEW community, not the closed one — \
                 a dangling index here is the silent corruption spec §4.4 forbids"
            );
            assert_eq!(
                (
                    standing.assessment.to_bits(),
                    standing.last_seen_population.to_bits()
                ),
                (
                    terms.assessment.to_bits(),
                    terms.last_seen_population.to_bits()
                ),
                "…on exactly the terms it stood on: the {label}'s relation was CARRIED, not \
                 re-formed by some other rule"
            );
            // The one term a reseat DOES re-stamp: the lord's tenure. See
            // `a_patron_driven_off_its_land_keeps_its_vassals_and_arrives_
            // weakened` for why
            // (a fact may not predate either entity it names).
            assert_eq!(
                standing.since.to_bits(),
                RESEAT_YEAR.to_bits(),
                "the {label}'s `since` must be re-stamped to the day the new lord was \
                 seated ({RESEAT_YEAR}), not carried from the old one ({})",
                terms.since
            );
        }
        assert_eq!(
            (
                bake.tally.subordinations_formed,
                bake.tally.patronage_transfers
            ),
            (0, 0),
            "nothing was subjugated or transferred in arm A: both relations were carried"
        );
        assert_eq!(
            bake.strength(reseated).to_bits(),
            (LORD_POP * (1.0 - WAR_LOSS)).to_bits(),
            "the lord arrived wounded: the war took its share of the people and the hoard \
             died with the old community"
        );

        // ─── Arm B: the obligation does not ──────────────────────────────────
        bake.begin_epoch();
        let held_when_paying = bake.communities[runaway].population;
        assert!(
            held_when_paying * MIGRATE_SURVIVAL >= VIABLE_MIN,
            "precondition: the runaway must be able to survive the road, or it would endure \
             and this arm would pass on the wrong branch"
        );
        for (who, label) in [(stayer, "stayer"), (runaway, "runaway")] {
            assert!(
                bake.strength(reseated) <= bake.strength(who) * RAID_MARGIN,
                "precondition: the beaten lord must not be able to eat the {label} on the \
                 road, or arm A's carry would be undone by a raid"
            );
        }
        // `settle_revolts` is driven so the zeros below mean the revolt rule
        // RAN and declined — not that it was never asked. Nothing here may
        // pass on the other mechanism's back.
        bake.settle_revolts();
        assert_eq!(
            bake.tally.vassal_revolts, 0,
            "precondition: neither vassal may throw the lord off — this arm measures flight \
             alone"
        );
        let fleeing = bake.collect_tribute(RESEAT_YEAR, &era);
        assert_eq!(
            bake.tally.tribute_collection_events, 2,
            "precondition: BOTH carried relations must have been collected on, or the carry \
             in arm A was decorative"
        );
        bake.resolve_flights(fleeing, &era, RESEAT_YEAR);
        assert_eq!(
            bake.tally.vassal_flights, 1,
            "exactly one vassal was taxed past what its growth law could return"
        );
        assert!(
            !bake.communities[runaway].alive,
            "…and it closed its occupation and took the road"
        );
        let arrived = bake
            .communities
            .iter()
            .enumerate()
            .find(|(i, c)| *i != runaway && c.alive && c.lineage == runaway_lineage)
            .map(|(i, _)| i)
            .expect("the runaway must be standing somewhere else — it left, it did not die");
        assert_eq!(
            bake.tribute.get(&arrived).map(|t| t.patron),
            None,
            "THE RUNAWAY ARRIVES FREE: a relocating community drops its relations AS \
             SUBORDINATE (spec §4.3e). An obligation that followed it would make flight a \
             change of address rather than the escape §4.3d calls 'relocating rather than \
             continue in that condition'"
        );
        // The control. A rule that dissolved relations wholesale at the flight
        // pass — or one that dropped the carry the moment anything moved —
        // would satisfy the line above and fail here.
        assert_eq!(
            bake.tribute.get(&stayer).map(|t| t.patron),
            Some(reseated),
            "…while the vassal that STAYED still owes the same lord: the runaway's relation \
             ended because it left, not because relations were being dropped"
        );
        assert_eq!(
            bake.tribute.len(),
            1,
            "exactly one relation stands: the runaway's is gone from the table entirely, \
             not re-keyed onto some other index"
        );
        assert!(
            bake.communities[reseated].stores > 0.0,
            "the lord keeps what it already collected: losing a vassal costs it the stream, \
             not the hoard"
        );
    }

    #[test]
    fn the_low_root_is_exactly_where_the_opening_demand_stops_being_payable() {
        // `crash_basin_fraction` is derived, never written down, so what needs
        // pinning is the ALGEBRA and not the number: at that fraction of a
        // cell's capacity the vassal's logistic increment exactly equals the
        // opening demand, and a hair below it the demand wins. A value pinned
        // as 0.1464 would survive any error in the derivation; this does not.
        let x = crash_basin_fraction();
        // Evaluated per unit of `eff` — both sides scale with it, so the
        // relationship is a statement about the constants alone.
        let increment_at_root = GROWTH_RATE * x * (1.0 - x);
        assert!(
            (increment_at_root - ASSESS_RATE).abs() < 1.0e-12,
            "the low root {x} must be where the increment ({increment_at_root}) meets the \
             opening demand ({ASSESS_RATE})"
        );
        // The LOW root, not the high one: it must sit below maximum
        // sustainable yield, or the gate would exclude the whole farmable band.
        assert!(
            x > 0.0 && x < 0.5,
            "the low root {x} must lie strictly between extinction and MSY"
        );
        // …and it really is a boundary: just inside the basin the demand
        // exceeds everything the vassal can grow.
        let inside = x * 0.99;
        assert!(
            GROWTH_RATE * inside * (1.0 - inside) < ASSESS_RATE,
            "below the root the opening demand must exceed the increment, or the basin is \
             not a basin"
        );
    }

    #[test]
    fn the_size_gate_never_vetoes_an_eviction() {
        // The controller note, bound rather than trusted. §4.3b's gate decides
        // who may be MILKED, never who may be conquered: the eviction branch is
        // the shipped path and the size test sits after the covet test
        // precisely so it cannot reach it. Hoisting the gate above that test —
        // the obvious "simplification", since it reads nothing the covet test
        // writes — would make a long-horizon raider decline better LAND on
        // account of how few people stand on it, which is a different rule and
        // one this campaign never proposed.
        //
        // Same too-small target as the decline test, same generational patron;
        // only the raider's own cell is poorer, which is what turns the prize
        // from a mobile one into an immobile one.
        let (geo, graphs, capacity, river_prox, refugia, era) =
            cascade_world(|c| if c == CellId(0) { POOR } else { RICH });
        /// kobold's authored horizon — the longest sighted patron the shipped
        /// roster produces, and so the strictest size gate available.
        const GENERATIONAL: f64 = 0.8;
        let horizons: BTreeMap<KindId, f64> =
            [(KindId("goblin"), GENERATIONAL)].into_iter().collect();
        let mut bake = hand_bake(&graphs, &capacity, &river_prox, &refugia, no_disposition());
        bake.time_horizon = &horizons;
        let far = geo.neighbors(CellId(0))[0];
        let raider = bake.open(
            KindId("goblin"),
            CellId(0),
            0.0,
            40.0,
            Founding::Genesis(CellId(0)),
            None,
            0.0,
        );
        let target = bake.open(
            KindId("kobold"),
            far,
            0.0,
            DAUGHTER_POP,
            Founding::Genesis(far),
            None,
            0.0,
        );
        // The precondition that makes the result meaningful: this raider WOULD
        // have declined this same target as a vassal.
        assert!(
            bake.communities[target].population < bake.min_vassal(KindId("goblin"), 0, RICH),
            "precondition: the gate must bind on this pair ({} vs a minimum of {}), or the \
             eviction below proves only that the gate was never consulted",
            bake.communities[target].population,
            bake.min_vassal(KindId("goblin"), 0, RICH)
        );

        bake.maybe_raid(raider, &era, 0.0);

        assert_eq!(
            bake.tally.raided, 1,
            "better land must still be seized: the size gate may not veto an eviction"
        );
        assert_eq!(bake.tally.fled, 1, "the loser must still be driven off");
        assert!(
            bake.tribute.is_empty(),
            "an eviction takes the ground, never a tributary"
        );
    }

    #[test]
    fn an_unauthored_patron_is_read_at_the_neutral_middle() {
        // Controller note, and the one place this rule could silently become
        // its own opposite. Every other authored-psychology lookup in this
        // bake fails open to "unaffected" — an absent disposition does not
        // veto, an absent radius conceals nothing. On this axis there is no
        // unaffected value, and the tempting default (`0.0`, "no data, no
        // number") is the CRUELLEST patron in the family: it strips its vassal
        // to `FARM_FLOOR` and holds it there. A bake handed no psyche data
        // must behave like a median patron instead.
        let geo = Geosphere::new(1);
        let graphs = vec![full_land_graph(&geo)];
        let capacity = CellMap::from_fn(&geo, |_| RICH);
        let river_prox = CellMap::from_fn(&geo, |_| 0.0);
        let refugia = CellMap::from_fn(&geo, |_| false);
        let mut bake = hand_bake(&graphs, &capacity, &river_prox, &refugia, no_disposition());
        let none: BTreeMap<KindId, f64> = BTreeMap::new();
        bake.time_horizon = &none;

        let msy = RICH / 2.0;
        let neutral = FARM_FLOOR + NEUTRAL_HORIZON * (msy - FARM_FLOOR);
        assert_eq!(
            bake.target_stock(KindId("goblin"), 0, RICH).to_bits(),
            neutral.to_bits(),
            "an unauthored people must be read at the middle of the axis, not at its bottom"
        );
        assert!(
            bake.target_stock(KindId("goblin"), 0, RICH) > FARM_FLOOR,
            "…and the middle of the axis must not collapse onto the floor, which is what \
             'absent means zero' would silently mean here"
        );

        // A non-finite authored value falls back the same way, rather than
        // poisoning the setpoint (and with it the remittance) with a NaN.
        let broken: BTreeMap<KindId, f64> = [
            (KindId("goblin"), f64::NAN),
            (KindId("kobold"), f64::INFINITY),
            (KindId("bugbear"), f64::NEG_INFINITY),
        ]
        .into_iter()
        .collect();
        bake.time_horizon = &broken;
        for people in ["goblin", "kobold", "bugbear"] {
            assert_eq!(
                bake.target_stock(KindId(people), 0, RICH).to_bits(),
                neutral.to_bits(),
                "a non-finite horizon must read as the neutral middle, not propagate"
            );
        }
    }

    #[test]
    fn a_setpoint_never_sits_below_the_farm_floor() {
        // `FARM_FLOOR` is a floor, not an exemption (spec §4.2b/§8.3), and the
        // interpolation `FARM_FLOOR + h × (eff/2 − FARM_FLOOR)` breaches it on
        // its own: on land too poor to carry `2 × FARM_FLOOR`, `eff/2` sits
        // BELOW the floor, the bracket goes negative, and a long-horizon
        // patron would steer its vassal to a setpoint under the one bound
        // tribute may never cross. Marginal cells are exactly where a farmed
        // community is least able to survive it.
        let geo = Geosphere::new(1);
        let graphs = vec![full_land_graph(&geo)];
        let capacity = CellMap::from_fn(&geo, |_| RICH);
        let river_prox = CellMap::from_fn(&geo, |_| 0.0);
        let refugia = CellMap::from_fn(&geo, |_| false);
        let mut bake = hand_bake(&graphs, &capacity, &river_prox, &refugia, no_disposition());
        let horizons: BTreeMap<KindId, f64> = [(KindId("goblin"), 1.0)].into_iter().collect();
        bake.time_horizon = &horizons;

        // Land whose whole capacity is under twice the floor — the regime the
        // clamp exists for. Asserted to BE that regime, so the case cannot
        // stop being exercised without reddening.
        let marginal = FARM_FLOOR;
        assert!(
            marginal / 2.0 < FARM_FLOOR,
            "precondition: this cell's MSY ({}) must sit below the floor ({FARM_FLOOR}), or the \
             clamp is not being exercised",
            marginal / 2.0
        );
        assert_eq!(
            bake.target_stock(KindId("goblin"), 0, marginal).to_bits(),
            FARM_FLOOR.to_bits(),
            "a setpoint on marginal land must be raised back to the floor, never left below it"
        );
        // A dead cell (an era has made it worthless) is the degenerate case of
        // the same thing.
        assert_eq!(
            bake.target_stock(KindId("goblin"), 0, 0.0).to_bits(),
            FARM_FLOOR.to_bits(),
            "and a cell the era has killed must not put the setpoint at zero"
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
    // The assertion IS on two constants, and that is the point: the claim
    // under test is a relationship between them, which nothing else in the
    // file states. Moving it into a `const` block would trade the diagnostic
    // message — which names both values and what their ordering buys — for a
    // bare compile error.
    #[allow(clippy::assertions_on_constants)]
    fn the_assessment_can_actually_bind_against_the_logistic_ceiling() {
        // The largest surplus a subordinate's own land can ever yield is the
        // logistic increment at its peak, N = eff/2 → GROWTH_RATE/4 × eff. An
        // assessment above that is decorative: min(assessment, surplus) would
        // always take the surplus branch, and adapting it would change
        // nothing. The two constants are therefore COUPLED, and this pins the
        // relationship rather than either value, so a future change to
        // `GROWTH_RATE` reddens here instead of silently re-inerting §4.3.
        assert!(
            ASSESS_RATE < GROWTH_RATE / 4.0,
            "ASSESS_RATE {ASSESS_RATE} must sit below the logistic ceiling {}, or the demand \
             never binds",
            GROWTH_RATE / 4.0
        );
    }

    #[test]
    fn the_demand_binds_on_a_subordinate_at_peak_productivity() {
        // The relationship above, stated as behaviour rather than arithmetic:
        // a subordinate sitting exactly where its land is most productive
        // (N = eff/2, the maximum of the logistic increment) must hand over
        // its patron's WHOLE demand and still keep something — i.e. the
        // `assessment` branch of `min(assessment, surplus)` is the one taken.
        // Under the pre-Step-0 constant this world took the surplus branch and
        // the subordinate kept nothing, which is what made every later
        // adjustment to the assessment unobservable.
        let (geo, graphs, capacity, river_prox, refugia, era) = cascade_world(|_| RICH);
        let mut bake = hand_bake(&graphs, &capacity, &river_prox, &refugia, no_disposition());
        let far = geo.neighbors(CellId(0))[0];
        let patron = bake.open(
            KindId("goblin"),
            CellId(0),
            0.0,
            80.0,
            Founding::Genesis(CellId(0)),
            None,
            0.0,
        );
        // RICH / 2 — the peak of the logistic increment, where the surplus is
        // the largest this land will ever offer.
        let sub = bake.open(
            KindId("kobold"),
            far,
            0.0,
            RICH / 2.0,
            Founding::Genesis(far),
            None,
            0.0,
        );
        bake.tribute.insert(
            sub,
            Tribute {
                patron,
                assessment: RICH * ASSESS_RATE,
                since: 0.0,
                last_seen_population: RICH / 2.0,
            },
        );

        bake.begin_epoch();
        let before = bake.communities[sub].population;
        let pressure = bake.pressure_of(sub, &era);
        bake.grow(sub, &era, 0.0, pressure);
        let surplus = bake.communities[sub].population - before;
        bake.collect_tribute(0.0, &era);

        assert_eq!(
            bake.communities[patron].stores.to_bits(),
            (RICH * ASSESS_RATE).to_bits(),
            "the patron must receive its whole demand ({}), not merely the surplus ({surplus})",
            RICH * ASSESS_RATE
        );
        assert!(
            bake.communities[sub].population > before,
            "a subordinate at peak productivity must keep part of its increment: {} vs {before}",
            bake.communities[sub].population
        );
    }

    /// A patron holding one subordinate at the peak of its land's productivity
    /// (`N = eff/2`), with the relation formed through the real
    /// [`Bake::maybe_raid`] path so the memory the feedback reads is set the
    /// way the bake sets it, not the way a test would. Returns the pair and
    /// the assessment the relation opened at.
    fn adaptive_pair<'a>(
        geo: &Geosphere,
        graphs: &'a [ConnectionGraph],
        capacity: &'a CellMap<f64>,
        river_prox: &'a CellMap<f64>,
        refugia: &'a CellMap<bool>,
        era: &EraClimate,
    ) -> (Bake<'a>, usize, usize, f64) {
        let mut bake = hand_bake(graphs, capacity, river_prox, refugia, no_disposition());
        let far = geo.neighbors(CellId(0))[0];
        let patron = bake.open(
            KindId("goblin"),
            CellId(0),
            0.0,
            80.0,
            Founding::Genesis(CellId(0)),
            None,
            0.0,
        );
        let sub = bake.open(
            KindId("kobold"),
            far,
            0.0,
            RICH / 2.0,
            Founding::Genesis(far),
            None,
            0.0,
        );
        // 80 clears 50 × RAID_MARGIN, the land is value-flat (so the prize is
        // the people, not the ground) and the target is productive.
        bake.maybe_raid(patron, era, 0.0);
        let opened_at = bake
            .tribute
            .get(&sub)
            .expect("the pair must actually be in a relation")
            .assessment;
        (bake, patron, sub, opened_at)
    }

    /// The exogenous blow the adaptation tests hit a subordinate with: it
    /// loses a fifth of its people between one collection and the next.
    ///
    /// Chosen to be **survivable by the land alone**: at `(1 +
    /// GROWTH_RATE)² × SHOCK > 1` a community shocked every other epoch still
    /// recovers, so what the long run measures is the controller's stability
    /// and not a population dying of the disturbance itself. A harsher blow
    /// (0.6, tried first) empties the cell whatever the assessment does.
    ///
    /// It stands in for the population movers a one-pair fixture cannot fire —
    /// a war loss (`WAR_LOSS`, 0.3), a famine, a cell turned hostile.
    ///
    /// **It was load-bearing before amendment 3 and is merely a disturbance
    /// after it.** Under the superseded cap a remittance could not exceed the
    /// epoch's growth increment, so tribute alone could never lower a
    /// subordinate's population, the negative half of the health signal could
    /// only ever arrive from *outside* the tribute loop, and a test that did
    /// not supply it would have been measuring a one-signed rule and calling it
    /// a feedback. Since spec §4.2b a greedy patron shrinks its vassal by
    /// itself (`an_over_milked_vassal_shrinks_and_its_patron_eases_off` asserts
    /// exactly that, with no shock anywhere in it). The shock is kept here
    /// because these two tests are about the CONTROLLER's stability under a
    /// disturbance it does not cause, which is a different question from
    /// whether the loop closes.
    const SHOCK: f64 = 0.8;

    #[test]
    fn a_patron_raises_its_demand_on_a_vassal_that_grew_and_eases_it_on_one_that_shrank() {
        // Spec §4.3, the corrected mechanism and the whole of it: the patron
        // feeds back on its subordinate's HEALTH, which is two-signed by
        // construction. A vassal that grew can bear more; one that shrank is
        // being over-milked and the demand eases.
        //
        // Both directions are asserted in one test deliberately. The rule this
        // replaces (`assessment += shortfall × ADAPT_RATE`, with
        // `shortfall = assessment − remittance ≥ 0` by construction) was a
        // monotone ratchet, and a test that only checked the RISE would have
        // passed against it and proved nothing at all.
        //
        // The two arms differ in exactly one input — whether `SHOCK` fired
        // between the formation reading and the collection — so nothing but
        // the sign of the health signal can explain a difference between them.
        let (geo, graphs, capacity, river_prox, refugia, era) = cascade_world(|_| RICH);

        // (assessment after collection, the subordinate's population before /
        // after the epoch, what was remitted) per arm.
        let mut arms: Vec<(f64, f64, f64, f64)> = Vec::new();
        for shocked in [false, true] {
            let (mut bake, patron, sub, opened_at) =
                adaptive_pair(&geo, &graphs, &capacity, &river_prox, &refugia, &era);
            assert_eq!(
                opened_at.to_bits(),
                (RICH * ASSESS_RATE).to_bits(),
                "precondition: the relation must open at the assessed rate"
            );
            bake.begin_epoch();
            if shocked {
                bake.communities[sub].population *= SHOCK;
            }
            let seen_last = RICH / 2.0;
            let pressure = bake.pressure_of(sub, &era);
            bake.grow(sub, &era, 0.0, pressure);
            bake.collect_tribute(0.0, &era);
            let after = bake.communities[sub].population;
            let assessment = bake
                .tribute
                .get(&sub)
                .expect("the relation must still stand")
                .assessment;
            arms.push((
                assessment,
                seen_last,
                after,
                bake.communities[patron].stores,
            ));
        }
        let (grew_assessment, grew_before, grew_after, grew_remitted) = arms[0];
        let (shrank_assessment, shrank_before, shrank_after, _) = arms[1];

        // Preconditions, so neither arm can pass on a technicality: the
        // unshocked vassal must really have ended the epoch larger than the
        // patron last saw it, and the shocked one really smaller.
        assert!(
            grew_after > grew_before,
            "precondition: the unshocked vassal must have GROWN past what its patron last saw \
             ({grew_after} vs {grew_before}) — with a flat vassal the signal is zero and \
             neither arm means anything"
        );
        assert!(
            shrank_after < shrank_before,
            "precondition: the shocked vassal must have SHRUNK below what its patron last saw \
             ({shrank_after} vs {shrank_before})"
        );
        assert!(
            grew_remitted > 0.0,
            "precondition: tribute must have flowed in the growing arm ({grew_remitted})"
        );

        let opened_at = RICH * ASSESS_RATE;
        assert!(
            grew_assessment > opened_at,
            "a patron whose vassal grew must demand MORE next time: {grew_assessment} vs the \
             {opened_at} it opened at"
        );
        assert!(
            shrank_assessment < opened_at,
            "a patron whose vassal shrank must EASE its demand: {shrank_assessment} vs the \
             {opened_at} it opened at. A one-signed error term is a ratchet, not a feedback \
             loop, and cannot produce a cycle (spec §4.3)."
        );
        assert!(
            shrank_assessment > 0.0,
            "and easing must not extinguish the demand: an assessment at exactly zero is an \
             ABSORBING state under a multiplicative rule ({shrank_assessment})"
        );

        // The size of the correction, not merely its sign: the demand moves in
        // proportion to the RELATIVE change in the vassal's population, scaled
        // by `ADAPT_RATE`. Without this a rule that read the absolute headcount
        // change, or that moved by a fixed step, would pass on direction alone.
        for (assessment, before, after, arm) in [
            (grew_assessment, grew_before, grew_after, "grew"),
            (shrank_assessment, shrank_before, shrank_after, "shrank"),
        ] {
            let signal = (after - before) / before;
            let expected = opened_at + signal * opened_at * ADAPT_RATE;
            assert!(
                (assessment - expected).abs() < 1.0e-12,
                "{arm} arm: the correction must be ADAPT_RATE × the relative change in the \
                 vassal's population ({signal}) applied to the standing demand: expected \
                 {expected}, got {assessment}"
            );
        }
    }

    #[test]
    fn the_patron_measures_against_its_last_visit_not_against_the_conquest() {
        // The memory must be REFRESHED at every collection, and the two rules
        // are told apart by a vassal that is smaller than the patron left it
        // last time but still larger than it was on the day it was taken:
        //
        //   * measured against the last visit (the rule), the signal is
        //     NEGATIVE and the demand eases;
        //   * measured against the conquest (a memory frozen at formation),
        //     the same epoch reads POSITIVE and the demand climbs.
        //
        // Opposite signs, so this cannot be satisfied by accident. Without it
        // the refresh could be deleted with the whole suite still green — the
        // patron would then be reacting to a reading years stale, which on a
        // long-lived relation is a different mechanism wearing the same name.
        let (geo, graphs, capacity, river_prox, refugia, era) = cascade_world(|_| RICH);
        let (mut bake, _patron, sub, opened_at) =
            adaptive_pair(&geo, &graphs, &capacity, &river_prox, &refugia, &era);
        let at_conquest = bake.communities[sub].population;

        // Epoch 1: a quiet year. The vassal grows and the demand rises.
        bake.begin_epoch();
        let pressure = bake.pressure_of(sub, &era);
        bake.grow(sub, &era, 0.0, pressure);
        bake.collect_tribute(0.0, &era);
        let after_first = bake.communities[sub].population;
        let assessment_first = bake
            .tribute
            .get(&sub)
            .expect("the relation must stand")
            .assessment;
        assert!(
            after_first > at_conquest && assessment_first > opened_at,
            "precondition: the first epoch must leave the vassal larger than it was taken \
             ({after_first} vs {at_conquest}) and the demand higher than it opened at \
             ({assessment_first} vs {opened_at})"
        );

        // Epoch 2: a blow — the same kind of exogenous loss `SHOCK` stands in
        // for — sized so the vassal ends BETWEEN the two readings.
        bake.begin_epoch();
        bake.communities[sub].population = 48.0;
        let pressure = bake.pressure_of(sub, &era);
        bake.grow(sub, &era, 25.0, pressure);
        bake.collect_tribute(25.0, &era);
        let after_second = bake.communities[sub].population;
        let assessment_second = bake
            .tribute
            .get(&sub)
            .expect("the relation must stand")
            .assessment;

        assert!(
            after_second < after_first && after_second > at_conquest,
            "precondition: the second epoch must leave the vassal SMALLER than the patron last \
             left it ({after_second} vs {after_first}) but LARGER than it was at conquest \
             ({at_conquest}) — otherwise the two readings agree and nothing is being told apart"
        );
        assert!(
            assessment_second < assessment_first,
            "the patron must read its vassal against its LAST VISIT ({after_first}), where the \
             signal is negative, not against the conquest ({at_conquest}), where the same epoch \
             reads as growth: the demand must ease from {assessment_first}, got \
             {assessment_second}"
        );
    }

    #[test]
    fn no_patron_may_demand_more_than_the_land_could_ever_produce() {
        // Spec §4.5's divergence bound, stated where the adaptive loop can
        // actually reach it. The clamp at formation never binds (`ASSESS_RATE`
        // is far under `ASSESS_MAX`), so before adaptation existed nothing in
        // the suite could tell a clamped write from an unclamped one — and
        // deleting the clamp left every test green. A patron returning to find
        // its vassal several times the size it left is the case that reaches
        // the ceiling in one step.
        let (geo, graphs, capacity, river_prox, refugia, era) = cascade_world(|_| RICH);
        let mut bake = hand_bake(&graphs, &capacity, &river_prox, &refugia, no_disposition());
        let far = geo.neighbors(CellId(0))[0];
        let patron = bake.open(
            KindId("goblin"),
            CellId(0),
            0.0,
            80.0,
            Founding::Genesis(CellId(0)),
            None,
            0.0,
        );
        let sub = bake.open(
            KindId("kobold"),
            far,
            0.0,
            RICH / 2.0,
            Founding::Genesis(far),
            None,
            0.0,
        );
        // A standing demand already high on the land, and a memory of a vassal
        // a tenth of the size the patron now finds.
        let standing = RICH * ASSESS_MAX * 0.9;
        bake.tribute.insert(
            sub,
            Tribute {
                patron,
                assessment: standing,
                since: 0.0,
                last_seen_population: RICH / 20.0,
            },
        );

        bake.begin_epoch();
        let pressure = bake.pressure_of(sub, &era);
        bake.grow(sub, &era, 0.0, pressure);
        bake.collect_tribute(0.0, &era);

        let ceiling = RICH * ASSESS_MAX;
        let now = bake.communities[sub].population;
        let signal = (now - RICH / 20.0) / (RICH / 20.0);
        let unclamped = standing + signal * standing * ADAPT_RATE;
        assert!(
            unclamped > ceiling,
            "precondition: the correction must actually reach past the ceiling, or the clamp \
             is not the thing being read ({unclamped} vs {ceiling})"
        );
        assert_eq!(
            bake.tribute
                .get(&sub)
                .expect("the relation must still stand")
                .assessment
                .to_bits(),
            ceiling.to_bits(),
            "the demand must be held at eff_capacity × ASSESS_MAX ({ceiling}), not the \
             {unclamped} the correction asked for"
        );
    }

    #[test]
    fn a_long_run_assessment_neither_diverges_nor_absorbs_at_zero() {
        // Spec §4.5's Lorenz claim, DEMONSTRATED. A first-order feedback with
        // delay — which is exactly what §4.3 is, the delay being the epoch
        // step — period-doubles into chaos above a critical gain, so
        // `ADAPT_RATE`'s bound may not be asserted in a comment.
        //
        // The multiplicative form carries a second failure mode the additive
        // one did not: an assessment at exactly `0.0` is ABSORBING
        // (`signal × 0 × ADAPT_RATE == 0` forever), so a gain large enough to
        // overshoot through zero kills the relation's demand permanently
        // rather than making it diverge. Both are checked.
        //
        // The subordinate is shocked on alternating epochs (see `SHOCK`), so
        // the loop is driven in BOTH directions for the whole run rather than
        // settling onto the one-signed fixed point tribute alone would give it.
        let (geo, graphs, capacity, river_prox, refugia, era) = cascade_world(|_| RICH);
        let (mut bake, _patron, sub, opened_at) =
            adaptive_pair(&geo, &graphs, &capacity, &river_prox, &refugia, &era);

        /// Epochs driven — long enough for a period-doubling cascade to show
        /// itself rather than being hidden in a transient.
        const EPOCHS: usize = 200;
        /// Years per driven epoch (the bake's own default step).
        const EPOCH_YEARS: f64 = 25.0;
        let ceiling = RICH * ASSESS_MAX;

        let mut series: Vec<f64> = vec![opened_at];
        let mut populations: Vec<f64> = vec![bake.communities[sub].population];
        let (mut rose, mut eased) = (0_u32, 0_u32);
        for epoch in 0..EPOCHS {
            let year = epoch as f64 * EPOCH_YEARS;
            bake.begin_epoch();
            if epoch % 2 == 1 {
                bake.communities[sub].population *= SHOCK;
            }
            let pressure = bake.pressure_of(sub, &era);
            bake.grow(sub, &era, year, pressure);
            bake.collect_tribute(year, &era);
            let a = bake
                .tribute
                .get(&sub)
                .expect("the relation must stand for the whole run")
                .assessment;
            match a.total_cmp(series.last().expect("seeded")) {
                Ordering::Greater => rose += 1,
                Ordering::Less => eased += 1,
                Ordering::Equal => {}
            }
            series.push(a);
            populations.push(bake.communities[sub].population);
        }

        // Non-vacuity: the loop must have been driven in both directions, or
        // "bounded" is a statement about a series that never moved.
        assert!(
            rose > 0 && eased > 0,
            "precondition: the demand must have moved BOTH ways over the run (rose {rose} \
             times, eased {eased} times) — a one-signed series proves nothing about stability"
        );
        // The declared bound (spec §4.5).
        for (i, &a) in series.iter().enumerate() {
            assert!(
                a.is_finite() && (0.0..=ceiling).contains(&a),
                "epoch {i}: assessment {a} left [0, eff × ASSESS_MAX] = [0, {ceiling}]"
            );
            assert!(
                a > 0.0,
                "epoch {i}: the assessment reached exactly zero — a multiplicative rule can \
                 never leave it again (an absorbing state, spec §4.5)"
            );
        }
        // Bounded, and bounded by the DYNAMICS rather than by the clamp: an
        // assessment pinned to the ceiling would satisfy the bound above while
        // the loop underneath it had run away.
        let peak = series
            .iter()
            .copied()
            .max_by(f64::total_cmp)
            .expect("non-empty");
        assert!(
            peak < ceiling,
            "the assessment rode its clamp ({peak} vs the {ceiling} ceiling): the bound must \
             come from the feedback, not from the guard-rail behind it"
        );
        // Non-divergent: the oscillation's amplitude in the tail may not exceed
        // its amplitude in the head. A period-doubling cascade shows up here as
        // a growing relative step.
        let step = |w: &[f64]| -> f64 {
            w.windows(2)
                .map(|p| ((p[1] - p[0]) / p[0]).abs())
                .max_by(f64::total_cmp)
                .unwrap_or(0.0)
        };
        let quarter = series.len() / 4;
        let head = step(&series[..quarter]);
        let tail = step(&series[series.len() - quarter..]);
        // The measured series settles onto a clean two-cycle whose head and
        // tail amplitudes agree to ~1e-16, so the comparison is made at a
        // relative tolerance rather than exactly: what is being rejected is
        // GROWTH, and float noise on an unchanging amplitude is not growth.
        const AMPLITUDE_EPS: f64 = 1.0e-9;
        assert!(
            tail <= head * (1.0 + AMPLITUDE_EPS),
            "the assessment's oscillation GREW over the run (head {head}, tail {tail}): a \
             first-order feedback with delay does that above its critical gain, and \
             ADAPT_RATE = {ADAPT_RATE} must sit below it"
        );
        // The subordinate's fate is deliberately NOT asserted here, and the
        // omission is a measurement rather than an oversight. Under a
        // disturbance that never stops, the population's fate is decided by
        // the disturbance and not by the controller: this fixture's vassal
        // decays toward zero at EVERY gain tried (it reaches 3.3e-8 at
        // ADAPT_RATE = 0.2 and 41 at a gain of 5 — the harsher gain "saves" it
        // only by annihilating the demand), so a survival assertion here would
        // pin the metronome, not the loop. What
        // this run leaves recorded is the shape of the trap the pre-amendment
        // rule had: a vassal held below the size at which its own increment
        // covered the standing demand was milked exactly flat, and a flat
        // vassal emits signal `0.0`, so the demand stopped easing. Spec §4.2b
        // is the answer to that trap — a vassal below that size is now bled
        // instead of held flat, so it emits a NEGATIVE signal and the demand
        // eases — and the trap survives here only in the degenerate corner
        // where the vassal is already sitting on `FARM_FLOOR` with nothing
        // above it to give. Survival at the scale the model claims it is owned
        // by `no_subordinate_is_farmed_below_the_farm_floor_by_tribute`, over a
        // fixture with no exogenous blow in it at all.
        let low = populations
            .iter()
            .copied()
            .min_by(f64::total_cmp)
            .expect("non-empty");
        assert!(
            low.is_finite() && low >= 0.0,
            "a population may fall under a disturbance, but never below zero or out of the \
             reals: {low}"
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
                    last_seen_population: bake.communities[s].population,
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
        let (r_id, r_lineage, r_disposition) = (
            bake.communities[roller].id,
            bake.communities[roller].lineage,
            bake.communities[roller].disposition,
        );
        bake.close(roller, 0.0, CauseOfEnd::Fled, Ended::Nature);

        let outcome = bake.relocate_holding_nothing(
            KindId("kobold"),
            50.0,
            r_lineage,
            r_id,
            0.0,
            CellId(0),
            &era,
            0.0,
            0,
            r_disposition,
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
            bake.records[bake.communities[seated].record].core.people,
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
            bake.records[bake.communities[holder].record].core.cause,
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
            let (r_id, r_lineage, r_disposition) = (
                bake.communities[roller].id,
                bake.communities[roller].lineage,
                bake.communities[roller].disposition,
            );
            bake.close(roller, 0.0, CauseOfEnd::Fled, Ended::Nature);
            let outcome = bake.relocate_holding_nothing(
                KindId("kobold"),
                roller_pop,
                r_lineage,
                r_id,
                0.0,
                CellId(0),
                &era,
                0.0,
                0,
                r_disposition,
            );
            let seated = *bake
                .node_index
                .get(&CellId(20))
                .expect("the rich cell is occupied either way");
            let holder_people = bake.records[bake.communities[seated].record].core.people;
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
        let (r_id, r_lineage, r_disposition) = (
            bake.communities[roller].id,
            bake.communities[roller].lineage,
            bake.communities[roller].disposition,
        );
        bake.close(roller, 0.0, CauseOfEnd::Fled, Ended::Nature);

        let outcome = bake.relocate_holding_nothing(
            KindId("kobold"),
            50.0,
            r_lineage,
            r_id,
            0.0,
            CellId(0),
            &era,
            0.0,
            0,
            r_disposition,
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
            bake.records[bake.communities[seated].record].core.people,
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
        let (r_id, r_lineage, r_disposition) = (
            bake.communities[roller].id,
            bake.communities[roller].lineage,
            bake.communities[roller].disposition,
        );
        bake.close(roller, 0.0, CauseOfEnd::Fled, Ended::Nature);

        let outcome = bake.relocate_holding_nothing(
            KindId("kobold"),
            50.0,
            r_lineage,
            r_id,
            0.0,
            CellId(0),
            &era,
            0.0,
            0,
            r_disposition,
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
        let (r_id, r_lineage, r_disposition) = (
            bake.communities[roller].id,
            bake.communities[roller].lineage,
            bake.communities[roller].disposition,
        );
        bake.close(roller, 0.0, CauseOfEnd::Fled, Ended::Nature);

        let outcome = bake.relocate_holding_nothing(
            KindId("kobold"),
            50.0,
            r_lineage,
            r_id,
            0.0,
            CellId(0),
            &era,
            0.0,
            0,
            r_disposition,
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
        let (r_id, r_lineage, r_disposition) = (
            bake.communities[roller].id,
            bake.communities[roller].lineage,
            bake.communities[roller].disposition,
        );
        bake.close(roller, 0.0, CauseOfEnd::Fled, Ended::Nature);

        // Strength 3.0 does not clear 5.0 × RAID_MARGIN, so cell 20 is not an
        // option however rich it is.
        let outcome = bake.relocate_holding_nothing(
            KindId("kobold"),
            3.0,
            r_lineage,
            r_id,
            0.0,
            CellId(0),
            &era,
            0.0,
            0,
            r_disposition,
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
            let (r_id, r_lineage, r_disposition) = (
                bake.communities[roller].id,
                bake.communities[roller].lineage,
                bake.communities[roller].disposition,
            );
            bake.close(roller, 0.0, CauseOfEnd::Fled, Ended::Nature);
            let outcome = bake.relocate_holding_nothing(
                KindId("kobold"),
                400.0,
                r_lineage,
                r_id,
                0.0,
                CellId(0),
                &era,
                0.0,
                0,
                r_disposition,
            );
            // Read everything the assertions need out of the borrowed `Bake`
            // before it dies with this closure's frame: the outcome, the
            // tallies, who ended up seated on each rung, and whether the
            // bottom of the ladder is still anywhere in the world.
            let seated = LADDER.map(|cell| {
                bake.node_index
                    .get(&cell)
                    .map(|&i| bake.records[bake.communities[i].record].core.people)
            });
            let terminal_survived = bake
                .communities
                .iter()
                .any(|c| c.alive && bake.records[c.record].core.people == TERMINAL);
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
        let (r_id, r_lineage, r_disposition) = (
            bake.communities[roller].id,
            bake.communities[roller].lineage,
            bake.communities[roller].disposition,
        );
        bake.close(roller, 0.0, CauseOfEnd::Fled, Ended::Nature);
        let records_before = bake.records.len();

        // (a) AT the cap: nothing may happen at all.
        let capped = bake.relocate_holding_nothing(
            KindId("kobold"),
            50.0,
            r_lineage,
            r_id,
            0.0,
            CellId(0),
            &era,
            0.0,
            CASCADE_DEPTH_CAP,
            r_disposition,
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
        let outcome = bake.relocate_holding_nothing(
            KindId("kobold"),
            50.0,
            r_lineage,
            r_id,
            0.0,
            CellId(0),
            &era,
            0.0,
            CASCADE_DEPTH_CAP - 1,
            r_disposition,
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
            let (r_id, r_lineage, r_disposition) = (
                bake.communities[roller].id,
                bake.communities[roller].lineage,
                bake.communities[roller].disposition,
            );
            bake.close(roller, 0.0, CauseOfEnd::Fled, Ended::Nature);
            bake.relocate_holding_nothing(
                KindId("kobold"),
                200.0,
                r_lineage,
                r_id,
                0.0,
                CellId(0),
                &era,
                0.0,
                0,
                r_disposition,
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
        // Inhibition 2 of spec §4.2a (durable): a community whose
        // `threat_response` falls below `RAID_DISPOSITION_MIN` never takes the
        // initiative, however strong it is on paper. Both arms are the same
        // world with the same populations — only the raider's disposition
        // differs — and the third arm pins the fail-open contract for a people
        // with no authored psyche at all.
        //
        // `hand_bake` fixes the spread at zero, so each community draws its
        // people's authored location exactly and this stays the per-people
        // reading it was written as. What The Tolerance changed — that two
        // settlements of ONE people can answer differently — is the separate
        // claim of `two_settlements_of_one_people_can_differ_in_raiding`; this
        // test's job is still the veto itself.
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

    /// **The Tolerance's headline: the sorting phenomenon.** With a dispersion
    /// above zero, one people's settlements must not all answer the raid gate
    /// the same way — warlikeness is a property of a *place*, not of a *kind*.
    ///
    /// Both arms are the same people, the same authored temperament, the same
    /// world, the same populations and the same site; only the founding year
    /// differs between samples, so only the draw key does. The second arm is
    /// the control that makes the first arm's variation attributable: at
    /// dispersion 0 every sample draws its people's authored location exactly,
    /// so a uniform answer there proves the year is not doing the work by some
    /// other route (tech horizon, say) and the spread is.
    ///
    /// Written against the pre-change gate it fails on arm 1 with every sample
    /// answering 1 — which is precisely the premise this campaign exists to
    /// overturn.
    #[test]
    fn two_settlements_of_one_people_can_differ_in_raiding() {
        let probe = Geosphere::new(1);
        let target_cell = probe.neighbors(CellId(0))[0];
        // Authored exactly AT the gate, so a symmetric spread puts roughly half
        // the draws on each side of it and the test is not fishing for a rare
        // tail.
        let authored: BTreeMap<KindId, f64> = [(KindId("kobold"), 0.6)].into_iter().collect();
        let spread: BTreeMap<KindId, f64> = [(KindId("kobold"), 0.3)].into_iter().collect();
        let raided_when_founded_in = |spread: &BTreeMap<KindId, f64>, founded: f64| -> u64 {
            let (_geo, graphs, capacity, river_prox, refugia, era) =
                cascade_world(|c| if c == target_cell { 110.0 } else { 100.0 });
            let mut bake =
                hand_bake_spread(&graphs, &capacity, &river_prox, &refugia, &authored, spread);
            let raider = bake.open(
                KindId("kobold"),
                CellId(0),
                founded,
                200.0,
                Founding::Genesis(CellId(0)),
                None,
                0.0,
            );
            bake.open(
                KindId("goblin"),
                target_cell,
                founded,
                50.0,
                Founding::Genesis(target_cell),
                None,
                0.0,
            );
            bake.maybe_raid(raider, &era, founded);
            bake.tally.raided
        };

        // The default bake grid's first twenty epochs (0, 25, … 475).
        let years: Vec<f64> = (0..20).map(|i| f64::from(i) * 25.0).collect();

        let dispersed: Vec<u64> = years
            .iter()
            .map(|&y| raided_when_founded_in(&spread, y))
            .collect();
        assert!(
            dispersed.contains(&1),
            "no settlement of this people took the initiative anywhere: {dispersed:?}"
        );
        assert!(
            dispersed.contains(&0),
            "every settlement of this people answered the gate identically \
             ({dispersed:?}) — warlikeness is still a property of the KIND"
        );

        // The control: same people, same authored 0.6, same years, no spread.
        let uniform: Vec<u64> = years
            .iter()
            .map(|&y| raided_when_founded_in(no_spread(), y))
            .collect();
        assert!(
            uniform.iter().all(|&r| r == 1),
            "at dispersion 0 every settlement must answer its people's authored \
             temperament identically, or the founding year is moving something \
             other than the draw: {uniform:?}"
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
            let (r_id, r_lineage, r_disposition) = (
                bake.communities[roller].id,
                bake.communities[roller].lineage,
                bake.communities[roller].disposition,
            );
            bake.close(roller, 0.0, CauseOfEnd::Fled, Ended::Nature);
            bake.relocate_holding_nothing(
                KindId("kobold"),
                50.0,
                r_lineage,
                r_id,
                0.0,
                CellId(0),
                &era,
                0.0,
                0,
                r_disposition,
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
