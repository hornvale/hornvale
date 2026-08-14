//! The hazard field (The Repose, spec §3.1): how often a cell's ground acts.
//!
//! **It does not accumulate.** No stress builds toward a threshold and no
//! state carries between calls: `hazard_at` is a pure read over terrain's
//! committed fields, and asking twice can only ever give the same answer.
//! This is constitutional rather than a simplification — `BIO-36` fixes
//! tier-0 as a *drawn stationary regime*, "invented on demand and narrated
//! backwards, never forward-simulated", and the Lorenz guard-rail forbids
//! the forward-integrator alternative outright. The timeline of a Hornvale
//! catastrophe has no left half, and that is the design.
//!
//! **Every constant here is authored, not fitted** ("models author, dice
//! roll"). Each carries the reasoning for its value in its own doc comment.
//! They are the analytically-known regime `TOOL-analytic-limiting-case`
//! asks for: we put the law in by hand, so recovering it downstream proves
//! the implementation and never the world (spec §3.3).
//!
//! # The event stream (spec §3.3)
//!
//! [`events_in`] turns a recurrence into events. The rate is stationary, the
//! process is Poisson, and the sizes come from authored laws —
//! Gutenberg-Richter for earthquakes, a VEI-shaped law for eruptions — so
//! there is nothing to accumulate and nothing to integrate forward. **The
//! window a caller asks about is a filter, never a key**: draws are keyed on
//! a block of a fixed lattice tiling world time, so the event sequence of a
//! `(seed, cell)` exists independently of who asks about it and a narrower
//! query returns a subset of a wider one rather than an unrelated set. See
//! [`events_in`] for what that buys and `crate::streams::HAZARD_EVENT` for
//! the key.
//!
//! **The laws were authored; the numbers below are the check that the draw
//! carries them, and are not a finding.** Recovered on 2026-08-14 from a
//! seed-42 level-5 globe (`windows/worldgen/tests/repose_laws.rs`, which
//! states the tolerance arithmetic):
//!
//! | authored | recovered |
//! |---|---|
//! | `B_VALUE` = 1.0 | b = 0.99935 (199,891 events, busiest cell) and 1.00162 (49,842 events, quietest) |
//! | recurrence 276.346 y | mean interval 276.495 y |
//! | recurrence 17,498.865 y | mean interval 17,435.032 y |
//! | recurrence 19,999.934 y | mean interval 20,063.645 y |
//! | the VEI law's closed-form mean 2.62004 | mean VEI 2.61744 (50,190 eruptions) |
//!
//! Every one of those pairs is a number compared against the number that was
//! put in by hand. Nothing in the table says anything about volcanism or
//! seismicity; it says the plumbing between the field and the events is
//! sound, which is exactly what `TOOL-analytic-limiting-case` is for.
//!
//! **Why no ETAS, no aftershocks, no triggering term.** A stated non-goal
//! (spec §2.2 and §7), and not for cost: ETAS is a branching process whose
//! control parameter is σ, and `SOC-criticality` has already been falsified
//! twice on this project (The Tumult, σ ≈ 0.051; The Tithe, σ ≈ 0.11 with the
//! shape unmoved). Adding one here would be the same experiment on the same
//! statistic a third time, dressed as a feature.
//!
//! **Why no second `BoundaryKind` match lives here.** Boundary kind is a
//! stated input of the field, and it reaches this module through the two
//! readings that already carry it, not through a match of its own:
//! `unrest_at` is *`intensity(kind)` × closing speed × youth × proximity*
//! (`terrain::elevation::generate_unrest`), and edifice presence is
//! island-arc-only by construction. A per-kind factor applied on top of
//! unrest would count the kind twice — an unauthored fudge with no
//! physical claim behind it. What kind *should* select is the shape of the
//! magnitude law, and that is the event draw's business (spec §3.3), not
//! the mean interval's.

use hornvale_kernel::seed::StreamLabel;
use hornvale_kernel::{CellId, Seed, Stream, WorldTime, Years, math};
use hornvale_terrain::GeneratedTerrain;

/// How often a cell's ground acts, as mean intervals between events.
///
/// A STEADY rate, never an accumulating stress (spec §3.1): nothing here
/// carries state between events, and the timeline of a Hornvale catastrophe
/// has no left half. That is the design, forced by `BIO-36`'s tier-0 rule
/// and the Lorenz guard-rail, not a shortcut.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Recurrence {
    /// Mean interval between seismic events at or above the catalogue's
    /// lower magnitude cutoff.
    ///
    /// This is the rate the events at this cell are actually drawn at:
    /// seismicity is per-cell, so field and draw agree everywhere.
    pub seismic: Years,
    /// **The volcanic field's LOCAL value at this cell** — the eruption
    /// interval this cell's own `unrest` maps to — or `None` where there is
    /// no edifice.
    ///
    /// # Off-source this is NOT the rate eruptions are drawn at
    ///
    /// Read this before using it as "how often this place erupts". An
    /// edifice spans one or two cells and belongs to **one mountain**, whose
    /// recurrence is sampled once at the edifice's source contact — the same
    /// sample-once-at-the-source shape terrain uses for the arc gate itself
    /// (`crate::volcano`'s module doc). [`events_in`] therefore draws
    /// eruptions at `volcano_at(seed, terrain, cell).recurrence`, which is
    /// this field read **at the source**, not here. On a flank cell the two
    /// disagree.
    ///
    /// **The rate eruptions actually happen at is
    /// `volcano_at(seed, terrain, cell).recurrence`.** A consumer asking how
    /// often the ground under a settlement erupts — Task 7's knownness
    /// half-life is exactly this question — wants that, not this field.
    ///
    /// Measured on 2026-08-14 (`hazard.rs`'s own module tests hold the shape
    /// of it; these are the counts behind them):
    ///
    /// | globe | cones | multi-cell | non-source cells | disagreeing | worst ratio |
    /// |---|---|---|---|---|---|
    /// | seed 42, L6 | 187 | 114 | 173 | 173 | 1.9885 |
    /// | seed 43, L6 | 173 | 109 | 168 | 168 | 2.2977 |
    /// | seed 42, L5 | 89 | 47 | 76 | 76 | 1.8914 |
    /// | seed 43, L5 | 89 | 48 | 82 | 82 | 2.2811 |
    ///
    /// So **every** off-source edifice cell disagrees, by up to a factor of
    /// about 2.3 on the globes measured. The disagreement is one-sided and
    /// that is structural rather than lucky: within one cone all of `unrest`'s
    /// factors but proximity are the contact's own, proximity decays away from
    /// the boundary, and `geometric_years` is strictly decreasing in `unrest`,
    /// so a flank cell's local value is always the *quieter* one.
    /// `off_source_the_local_field_is_never_more_active_than_the_drawn_rate`
    /// holds that, and
    /// `an_edifice_sources_field_value_is_the_rate_its_eruptions_are_drawn_at`
    /// pins the equality where it does hold.
    ///
    /// # Why the field is still defined off-source
    ///
    /// Because it is a *field*, and a cone's flank is volcanic ground: the
    /// value answers "how volcanic is it here", which is a different question
    /// from "how often does that mountain go off". Making it `None` off-source
    /// would say the flank of a volcano is not volcanic, and would break the
    /// property Task 4 deliberately established — that a volcanic recurrence
    /// exists exactly where an edifice does. The defect was never that the
    /// field exists; it was that its one-line doc described it as the other
    /// quantity.
    pub volcanic: Option<Years>,
}

/// Mean interval between seismic events in the quietest ground there is —
/// an old plate interior, far from any boundary (`unrest` = 0).
///
/// AUTHORED, from the terrestrial analogue: stable cratonic interiors do
/// produce damaging earthquakes (New Madrid, Charleston), but a *given*
/// patch of one waits tens of millennia between them. 20,000 years puts the
/// quiet end firmly outside any culture's living memory, which is the
/// property Task 7's knownness half-life will lean on: the field's quiet
/// end must be forgettable, or there is nothing to forget.
const SEISMIC_QUIET_YEARS: f64 = 20_000.0;

/// Mean interval between seismic events on the most active ground there is
/// (`unrest` = 1: a young plate closing fast, on the boundary itself).
///
/// AUTHORED, from the terrestrial analogue: the most active plate-boundary
/// segments deliver a cutoff-grade shock on a human generational scale —
/// once or twice a lifetime, felt by everyone who lives there. 30 years is
/// the short end of that, and deliberately *not* shorter: an interval below
/// a human generation would make the hazard ordinary weather rather than
/// the thing a people remembers and mis-remembers.
const SEISMIC_ACTIVE_YEARS: f64 = 30.0;

/// Mean interval between eruptions at a quiet edifice (`unrest` = 0).
///
/// AUTHORED: an arc cone on a slow margin is not extinct, it is *dormant* —
/// five millennia between eruptions, long enough that the mountain is
/// remembered as a mountain and not as a volcano. This is the case spec §1
/// is named for: Vesuvius in AD 79 "was not a known hazard; it was a
/// fertile hill with towns on its flanks".
const VOLCANIC_QUIET_YEARS: f64 = 5_000.0;

/// Mean interval between eruptions at a vigorously active edifice
/// (`unrest` = 1).
///
/// AUTHORED: the busiest arc volcanoes erupt on a scale of years, but the
/// eruption worth narrating — the one that ends a settlement rather than
/// dusting it — is the rarer, larger event. Two centuries is the interval
/// at which a people can plausibly hold the memory of the last one and
/// still be living on the flank.
const VOLCANIC_ACTIVE_YEARS: f64 = 200.0;

/// Interpolate a recurrence interval between its quiet and active ends,
/// geometrically in `unrest`.
///
/// Geometric, not linear, because recurrence spans orders of magnitude and
/// the physically meaningful step is a *factor*: halving the interval is
/// the same event whether it happens at 20,000 years or at 200. Linear
/// interpolation would spend almost the whole of `unrest`'s range inside
/// the quiet decade and compress every active regime into its last few
/// percent. Strictly decreasing in `unrest` for any `active < quiet`.
fn geometric_years(quiet_years: f64, active_years: f64, unrest: f64) -> Years {
    let years = quiet_years * math::powf(active_years / quiet_years, unrest);
    Years::new(years).expect("authored recurrence bounds are finite and positive")
}

/// Whether a cell carries a volcanic edifice — the gated island-arc cone
/// the elevation raised there.
///
/// A pure delegation to terrain's own derived read, kept here so the hazard
/// surface reads as one thing to Task 5. The read lives in `domains/terrain`
/// rather than being re-derived at this layer deliberately: an edifice is
/// terrain's concept, and a copy of the gate here could drift from the
/// elevation it is supposed to describe.
/// type-audit: bare-ok(flag: return)
pub fn has_edifice(terrain: &GeneratedTerrain, cell: CellId) -> bool {
    terrain.has_edifice(cell)
}

/// The hazard field at a cell: mean intervals between seismic events, and
/// between eruptions where there is an edifice to erupt from.
///
/// **A field read, and only that.** [`Recurrence::seismic`] is the rate this
/// cell's quakes are drawn at, but [`Recurrence::volcanic`] is the local field
/// value and is *not* the rate its eruptions are drawn at unless the cell is
/// its edifice's source contact — an eruption belongs to a mountain, and a
/// mountain samples the field once, at its source. For the rate that actually
/// governs events, ask `volcano_at(seed, terrain, cell).recurrence`. See
/// [`Recurrence::volcanic`] for the measured size of the gap.
///
/// Pure and stateless (spec §3.1). Composes exactly three shipped readings —
/// `unrest_at`, and the boundary kind and edifice presence that
/// [`has_edifice`] carries — and authors the mapping from them to intervals.
/// `unrest` is range-clamped to `[0,1]` because this module's monotonicity is
/// stated over that range and terrain already guarantees it
/// (`tectonic_properties.rs`'s `every_default_globe_satisfies_every_invariant`
/// asserts `(0.0..=1.0).contains(u)` for every cell of every swept globe).
///
/// **The clamp is a range guard for finite values, and deliberately not a
/// total one.** `f64::clamp` PROPAGATES NaN rather than pinning it, so a NaN
/// `unrest` would flow through `geometric_years` into
/// `Years::new(..).expect(..)` — which rejects non-finite values — and panic
/// there instead of being silently clamped to a plausible interval. That is
/// the intended behaviour, not an oversight: a NaN unrest is a defect in the
/// terrain field upstream, and the same range assertion above already fails
/// on it (`contains` is false for NaN). Pinning it here would convert a
/// loud upstream bug into a quiet 200-year recurrence.
pub fn hazard_at(terrain: &GeneratedTerrain, cell: CellId) -> Recurrence {
    let unrest = terrain.unrest_at(cell).clamp(0.0, 1.0);
    let seismic = geometric_years(SEISMIC_QUIET_YEARS, SEISMIC_ACTIVE_YEARS, unrest);
    let volcanic = has_edifice(terrain, cell)
        .then(|| geometric_years(VOLCANIC_QUIET_YEARS, VOLCANIC_ACTIVE_YEARS, unrest));
    Recurrence { seismic, volcanic }
}

/// Which of the field's two processes an event came from.
///
/// Spelled `HazardEventKind` and not `HazardKind`: the latter is taken by
/// [`crate::vestige::HazardKind`] (structural collapse, toxic gas,
/// pestilence, …), a shipped type re-exported from this crate's root. Task
/// 6's brief specified `HazardKind`; the collision is with the older,
/// exported name, so this one is the one that moves.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HazardEventKind {
    /// An earthquake at or above [`M_MIN`]. It carries no identity, and that
    /// is deliberate: negating "localized" yields a belt with no point of
    /// origin, which is what a quake is, and nobody names an earthquake
    /// ([`crate::volcano`]'s module doc).
    Seismic,
    /// An eruption at or above [`VEI_MIN`], of the volcano the cell belongs
    /// to — keyed on that mountain's identity rather than on the query cell,
    /// so the two halves of one cone share one eruption history.
    Eruption,
}

/// One thing the ground did, at a time.
///
/// Never committed and never stored: C0 writes no facts, and an event is
/// recomputed on demand from `(seed, cell)` exactly as the volcano it belongs
/// to is (decision 0100's recompute test puts both in the phenomenon
/// register).
/// type-audit: bare-ok(ratio: magnitude)
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct HazardEvent {
    /// When it happened, in absolute standard days.
    pub day: WorldTime,
    /// Which process produced it.
    pub kind: HazardEventKind,
    /// How big — a moment magnitude for [`HazardEventKind::Seismic`], a
    /// VEI-shaped size for [`HazardEventKind::Eruption`]. Both are
    /// logarithmic and dimensionless, and each is on its own scale: a 6 and a
    /// 6 are not the same event.
    pub magnitude: f64,
}

/// The Gutenberg-Richter `b`: each unit of magnitude is `10^-b` times as
/// frequent.
///
/// AUTHORED at 1.0, the value the empirical law takes for tectonic
/// seismicity almost everywhere on Earth — it is the reason the relation is
/// quoted as a law at all rather than as a per-region fit. **Not tuned to
/// anything**: `repose_laws.rs` compares the drawn sample against this
/// number, so if the two ever disagree it is the draw that is wrong, never
/// this constant.
/// type-audit: bare-ok(ratio)
pub const B_VALUE: f64 = 1.0;

/// The catalogue's lower magnitude cutoff — the smallest event the field
/// counts.
///
/// AUTHORED at 5.0, and tied to what [`Recurrence::seismic`] already means:
/// its interval is stated for "events at or above the catalogue's lower
/// magnitude cutoff", and the intervals themselves were authored around
/// *damaging* shocks — a cratonic interior's rare New-Madrid-grade event at
/// the quiet end, and at the busy end "a cutoff-grade shock ... felt by
/// everyone who lives there". M 5 is where ordinary construction starts to
/// fail, so it is the threshold that makes those two sentences true. A lower
/// cutoff would not be wrong physically, but it would silently redefine every
/// interval constant in this module.
/// type-audit: bare-ok(ratio)
pub const M_MIN: f64 = 5.0;

/// The largest earthquake the law can produce.
///
/// AUTHORED at 9.5: magnitude is bounded by fault dimensions, not by the
/// frequency law, and 9.5 is the largest instrumentally recorded event
/// (Valdivia, 1960). The law is **truncated** at this value rather than
/// clamped to it — the draw inverts the CDF of the law restricted to
/// `[M_MIN, M_MAX]` — so there is no point mass at the top, which a clamp
/// would have created and which would have made the biggest earthquake also
/// the most common one above M 9.
/// type-audit: bare-ok(ratio)
pub const M_MAX: f64 = 9.5;

/// The smallest eruption the field counts.
///
/// AUTHORED at VEI 2. Below it the scale describes continuous or effusive
/// activity — a lava lake, a steaming vent — which is a *state* of a
/// mountain rather than an event a chronicle records, and
/// [`Recurrence::volcanic`]'s interval was authored for "the eruption worth
/// narrating — the one that ends a settlement rather than dusting it".
/// type-audit: bare-ok(ratio)
pub const VEI_MIN: f64 = 2.0;

/// The largest eruption the law can produce.
///
/// AUTHORED at VEI 8, which is the top of the scale as defined: 8 is the
/// supervolcanic class (Toba, the Yellowstone eruptions) and nothing above it
/// has a name. Truncated, not clamped, for the same reason [`M_MAX`] is.
/// type-audit: bare-ok(ratio)
pub const VEI_MAX: f64 = 8.0;

/// The eruption law's decade-decay, the VEI analogue of [`B_VALUE`].
///
/// AUTHORED at 0.7 ≈ log10(5), from the Holocene record's rule of thumb that
/// each step up the VEI scale is roughly five times rarer than the one below.
/// Not fitted: `repose_laws.rs` checks the draw against this number.
/// type-audit: bare-ok(ratio)
pub const VEI_B: f64 = 0.7;

/// The length of one block of the event lattice, in years.
///
/// **A save-format contract**: changing it redraws every event in every
/// world, because it changes which draws are keyed where. It is *not* a
/// physical constant and it does not shape the statistics — a Poisson process
/// is exactly the superposition of independent Poisson counts on disjoint
/// intervals with conditional-uniform positions inside them, so the lattice
/// is invisible to the distribution and decides only cost. 1,000 years is
/// chosen between the two costs it trades: a query pays one draw per block it
/// spans (so longer blocks are cheaper for long windows), while a block's
/// expected count is `BLOCK_YEARS / recurrence`, at most 33 on the busiest
/// authored ground, and the count draw is linear in that.
const BLOCK_YEARS: f64 = 1_000.0;

/// [`BLOCK_YEARS`] in standard days — the lattice's actual pitch, since world
/// time is days.
const BLOCK_DAYS: f64 = BLOCK_YEARS * Years::DAYS_PER_YEAR;

/// The pitch of the event lattice: the span of world time one block of draws
/// covers.
///
/// Exposed because a caller cannot otherwise tell where a block boundary
/// falls, and one class of defect is only visible to someone who can —
/// `repose_laws.rs`'s sub-window property needs a cut that lands strictly
/// *inside* a block with events on both sides of it, and a test that could
/// not construct that case would have passed against a draw that filtered
/// before it drew (it did: the mutation went green until this was exposed).
/// Returns a [`Years`] rather than the bare constant so nothing downstream
/// has to guess the unit.
pub fn event_block_length() -> Years {
    Years::new(BLOCK_YEARS).expect("an authored, positive, finite block length")
}

/// Which block of the event lattice a day falls in. Blocks tile the whole
/// timeline, including before genesis: `WorldTime` admits negative days
/// deliberately, and a negative block index is an ordinary coordinate.
fn block_index(day: f64) -> i64 {
    (day / BLOCK_DAYS).floor() as i64
}

/// The name of a process inside [`event_key`].
fn process_label(kind: HazardEventKind) -> &'static str {
    match kind {
        HazardEventKind::Seismic => "seismic",
        HazardEventKind::Eruption => "eruption",
    }
}

/// The spelling of one block's key inside the [`crate::streams::HAZARD_EVENT`]
/// derivation.
///
/// A **save-format contract**, like [`crate::volcano`]'s `volcano_key`:
/// changing this string redraws every event in every world. Three components,
/// each prefixed by what it is, so a future key with a fourth cannot silently
/// collide with today's — a place in space, which process, and a place in
/// time. **No ordinal appears anywhere in it**, which is what lets a query
/// filter the sequence instead of generating it (decision 0102).
fn event_key(cell: CellId, kind: HazardEventKind, block: i64) -> String {
    format!(
        "cell/{}/process/{}/block/{}",
        cell.0,
        process_label(kind),
        block
    )
}

/// The stream one block of one process at one cell draws from.
fn event_stream(seed: Seed, cell: CellId, kind: HazardEventKind, block: i64) -> Stream {
    seed.derive(crate::streams::HAZARD_EVENT)
        .derive(StreamLabel::dynamic(&event_key(cell, kind, block)))
        .stream()
}

/// The largest count the block draw will return. Reached only through the
/// floating-point tail: the loop's cumulative probability converges to 1 and
/// terminates for any `u < 1`, but `cdf` can saturate a few ULP below a `u`
/// very close to 1, and this stops that spinning. At the authored constants
/// the busiest possible block has mean 33.3, so the cap sits ~30 standard
/// deviations out and cannot round off a real draw.
const MAX_EVENTS_PER_BLOCK: u32 = 10_000;

/// How many events fall in one block: the Poisson quantile at `u`, by CDF
/// inversion, consuming exactly one uniform.
fn poisson_count(u: f64, lambda: f64) -> u32 {
    let mut term = math::exp(-lambda);
    let mut cdf = term;
    let mut k = 0_u32;
    while u >= cdf && k < MAX_EVENTS_PER_BLOCK {
        k += 1;
        term *= lambda / f64::from(k);
        cdf += term;
    }
    k
}

/// A draw from the authored magnitude law on `[min, max]`: an exponential
/// whose frequency falls by `10^-b` per unit, **truncated** to the bracket
/// rather than clamped to it.
///
/// This is the inverse CDF of the restricted law, so the result is a proper
/// distribution on the bracket with no point mass at either end. `u = 0`
/// gives `min` and `u -> 1` gives `max`.
fn truncated_magnitude(u: f64, min: f64, max: f64, b: f64) -> f64 {
    let tail = math::powf(10.0, -b * (max - min));
    min - math::log10(1.0 - u * (1.0 - tail)) / b
}

/// The magnitude law of one process.
fn magnitude_of(kind: HazardEventKind, u: f64) -> f64 {
    match kind {
        HazardEventKind::Seismic => truncated_magnitude(u, M_MIN, M_MAX, B_VALUE),
        HazardEventKind::Eruption => truncated_magnitude(u, VEI_MIN, VEI_MAX, VEI_B),
    }
}

/// One process's events inside a window: a stationary Poisson process of mean
/// interval `recurrence`, read off the fixed block lattice keyed on `key`.
///
/// **The window is a filter and never a key.** Every block the window touches
/// is drawn in full — count, then all its days, then a magnitude for each of
/// them — and only then are the events outside the request discarded. Filtering
/// earlier would be cheaper and wrong: the stream position after a block would
/// depend on how many events survived, so the same block would yield different
/// magnitudes to two different queries, and a sub-window would stop being a
/// subset. That is the same defect one level down as keying the stream on the
/// window bounds.
fn process_events(
    seed: Seed,
    key: CellId,
    kind: HazardEventKind,
    recurrence: Years,
    window: (WorldTime, WorldTime),
) -> Vec<HazardEvent> {
    let (start, end) = (window.0.day(), window.1.day());
    if end <= start {
        return Vec::new();
    }
    let lambda = BLOCK_DAYS / recurrence.days();
    let mut events = Vec::new();
    for block in block_index(start)..=block_index(end) {
        let mut stream = event_stream(seed, key, kind, block);
        let count = poisson_count(stream.next_f64(), lambda);
        let block_start = block as f64 * BLOCK_DAYS;
        // `next_f64()` is in [0, 1), so a day belongs to its own block by
        // construction — with one theoretical exception, recorded rather than
        // guarded: for a large `block`, `block_start + u * BLOCK_DAYS` can
        // round up to exactly the next block's start, putting the event in a
        // block that did not draw it. It needs `u` within an ulp of 1 (P ~
        // 1e-15 per event) and costs, at worst, one event landing a moment
        // early. A guard would be a branch on every event of every query to
        // move an event by one ulp.
        let mut days: Vec<f64> = (0..count)
            .map(|_| block_start + stream.next_f64() * BLOCK_DAYS)
            .collect();
        days.sort_by(|a, b| a.total_cmp(b));
        for day in days {
            let magnitude = magnitude_of(kind, stream.next_f64());
            if day >= start && day < end {
                events.push(HazardEvent {
                    day: WorldTime::new(day).expect("a finite day inside a finite window"),
                    kind,
                    magnitude,
                });
            }
        }
    }
    events
}

/// Every hazard event at a cell inside a window, in time order.
///
/// The window is **half-open**, `[start, end)`, so two adjacent windows
/// partition their union with nothing duplicated and nothing lost. An empty
/// or reversed window yields no events.
///
/// # What makes this a read and not a simulation
///
/// The event sequence of a `(seed, cell)` exists whether or not anyone asks
/// about it: draws are keyed on a block of the fixed timeline lattice, never
/// on the request, so a narrower query returns exactly the wider query's
/// events that fall inside it —
/// `repose_laws.rs::a_sub_window_query_returns_exactly_the_enclosing_windows_events`
/// holds that over four seeds rather than leaving it to this paragraph. There
/// is no origin to walk from, no state carried between calls, and no
/// accumulation of anything toward anything (spec §3.1, `BIO-36`).
///
/// **No triggering term, deliberately.** Events are independent given the
/// rate: no aftershock sequence, no ETAS branching. That is a stated non-goal
/// (spec §2.2 and §7) rather than an omission — ETAS's control parameter is
/// σ, and `SOC-criticality` has already been falsified twice on this project
/// (The Tumult, σ ≈ 0.051; The Tithe, σ ≈ 0.11 with the shape unmoved), so
/// adopting it would be the same experiment on the same statistic a third
/// time.
///
/// The two processes draw from separate streams under the same label, so a
/// world's seismicity is unchanged by whether its cell has a volcano — and an
/// eruption is keyed on the **volcano's** identity (its source contact),
/// which is why every cell of one cone reports the same eruptions.
///
/// # Cost
///
/// One count draw per 1,000-year block the window spans, plus two draws per
/// event in those blocks. Long windows are linear in their own length, so the
/// caller owns that: nothing here caps or truncates a window, because a cap
/// would silently answer a different question than the one asked.
///
/// **The block loop is therefore unbounded, and a caller must not hand it an
/// unclamped span.** A window of ~1e12 days — the kind an
/// "everything since genesis" default or a user-supplied number produces —
/// iterates `1e12 / BLOCK_DAYS` ≈ **2.7e6** blocks (`BLOCK_DAYS` is 365,250,
/// not 1,000: the block is a thousand *years*), and each of those blocks pays
/// a count draw plus two draws per event it yields. That is slow enough to
/// look hung rather than to fail. Clamp the span at the call site to what the
/// question actually needs.
pub fn events_in(
    seed: Seed,
    terrain: &GeneratedTerrain,
    cell: CellId,
    window: (WorldTime, WorldTime),
) -> Vec<HazardEvent> {
    let recurrence = hazard_at(terrain, cell);
    let mut events = process_events(
        seed,
        cell,
        HazardEventKind::Seismic,
        recurrence.seismic,
        window,
    );
    if let Some(volcano) = crate::volcano::volcano_at(seed, terrain, cell) {
        events.extend(process_events(
            seed,
            volcano.source,
            HazardEventKind::Eruption,
            volcano.recurrence,
            window,
        ));
    }
    // Stable, so the tie-break on a shared day is seismic-before-eruption by
    // construction (the seismic events were pushed first) — deterministic
    // without a second sort key.
    events.sort_by(|a, b| a.day.day().total_cmp(&b.day.day()));
    events
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::Geosphere;
    use hornvale_terrain::{BoundaryKind, GeneratedTerrain, TerrainPins};
    use std::collections::BTreeMap;

    /// A globe small enough to build in a unit test and large enough to
    /// carry every boundary kind.
    fn globe() -> (Geosphere, GeneratedTerrain) {
        let geo = Geosphere::new(5);
        let outcome = hornvale_terrain::generate(Seed(42), &geo, &TerrainPins::default())
            .expect("default pins generate");
        let terrain = GeneratedTerrain::new(geo.clone(), outcome);
        (geo, terrain)
    }

    /// A transform boundary is documented as "unrest, little relief" —
    /// seismic, not volcanic. Direction: pins the KIND separation, not the
    /// magnitudes.
    #[test]
    fn a_transform_boundary_is_seismic_and_never_volcanic() {
        let (geo, terrain) = globe();
        let mut transforms = 0_u32;
        for cell in geo.cells() {
            if terrain.boundary_at(cell).map(|b| b.kind) != Some(BoundaryKind::Transform) {
                continue;
            }
            transforms += 1;
            let hazard = hazard_at(&terrain, cell);
            assert_eq!(
                hazard.volcanic, None,
                "{cell:?} is a transform boundary with a volcanic recurrence"
            );
            assert!(
                hazard.seismic.get() < SEISMIC_QUIET_YEARS,
                "{cell:?} is a transform boundary yet no more seismic than a dead interior"
            );
        }
        assert!(transforms > 0, "no transform boundary on the test globe");
    }

    /// Every cell without an edifice is amagmatic, and every cell with one
    /// erupts. The two halves of the volcanic option, over a whole globe.
    #[test]
    fn volcanic_recurrence_exists_exactly_where_an_edifice_does() {
        let (geo, terrain) = globe();
        let mut edifices = 0_u32;
        for cell in geo.cells() {
            let hazard = hazard_at(&terrain, cell);
            if has_edifice(&terrain, cell) {
                edifices += 1;
                assert!(
                    hazard.volcanic.is_some(),
                    "{cell:?} carries an edifice and no eruption interval"
                );
            } else {
                assert_eq!(
                    hazard.volcanic, None,
                    "{cell:?} erupts from an edifice it does not have"
                );
            }
        }
        assert!(edifices > 0, "no edifice on the test globe");
    }

    /// Recurrence is a pure function of the cell's fields: same inputs,
    /// same answer, every call, with no memory between calls.
    #[test]
    fn hazard_is_pure_and_carries_no_state() {
        let (geo, terrain) = globe();
        let cell = geo
            .cells()
            .max_by(|a, b| terrain.unrest_at(*a).total_cmp(&terrain.unrest_at(*b)))
            .expect("a non-empty globe");
        assert!(terrain.unrest_at(cell) > 0.0, "the scan found dead ground");
        let first = hazard_at(&terrain, cell);
        for _ in 0..100 {
            assert_eq!(hazard_at(&terrain, cell), first, "hazard accumulated state");
        }
    }

    /// Higher unrest means a SHORTER interval, monotonically. Direction:
    /// catches an inverted sign, which is the defect that would make the
    /// whole campaign measure backwards.
    #[test]
    fn higher_unrest_shortens_the_seismic_interval() {
        let (geo, terrain) = globe();
        let mut rows: Vec<(f64, f64)> = geo
            .cells()
            .map(|c| (terrain.unrest_at(c), hazard_at(&terrain, c).seismic.get()))
            .collect();
        rows.sort_by(|a, b| a.0.total_cmp(&b.0));
        let distinct = rows.windows(2).filter(|w| w[0].0 != w[1].0).count();
        assert!(
            distinct > 100,
            "only {distinct} distinct unrest values — too flat to test monotonicity"
        );
        for pair in rows.windows(2) {
            let (lo, hi) = (pair[0], pair[1]);
            if lo.0 < hi.0 {
                assert!(
                    hi.1 < lo.1,
                    "unrest {} -> {} did not shorten the interval: {} -> {}",
                    lo.0,
                    hi.0,
                    lo.1,
                    hi.1
                );
            } else {
                assert_eq!(hi.1, lo.1, "equal unrest gave unequal intervals");
            }
        }
        // The span is real, not a rounding artefact: the quietest and the
        // most active ground on one globe differ by orders of magnitude.
        let (quietest, busiest) = (rows[0].1, rows[rows.len() - 1].1);
        assert!(
            quietest / busiest > 10.0,
            "the field is nearly flat: {quietest} vs {busiest}"
        );
    }

    /// The authored ends are the ends: no cell can be quieter than the dead
    /// interior or busier than the most active belt.
    #[test]
    fn every_interval_lies_inside_its_authored_bracket() {
        let (geo, terrain) = globe();
        for cell in geo.cells() {
            let hazard = hazard_at(&terrain, cell);
            assert!(
                (SEISMIC_ACTIVE_YEARS..=SEISMIC_QUIET_YEARS).contains(&hazard.seismic.get()),
                "{cell:?} seismic {} escaped the authored bracket",
                hazard.seismic.get()
            );
            if let Some(volcanic) = hazard.volcanic {
                assert!(
                    (VOLCANIC_ACTIVE_YEARS..=VOLCANIC_QUIET_YEARS).contains(&volcanic.get()),
                    "{cell:?} volcanic {} escaped the authored bracket",
                    volcanic.get()
                );
            }
        }
    }

    /// Ten millennia from genesis — long enough that even the quietest
    /// ground is likely to act, short enough to be free.
    fn window() -> (WorldTime, WorldTime) {
        (
            WorldTime::GENESIS,
            WorldTime::new(10_000.0 * Years::DAYS_PER_YEAR).expect("finite"),
        )
    }

    /// The busiest cell on the test globe, and the quietest.
    fn extremes(geo: &Geosphere, terrain: &GeneratedTerrain) -> (CellId, CellId) {
        let mut cells: Vec<CellId> = geo.cells().collect();
        cells.sort_by(|a, b| {
            hazard_at(terrain, *a)
                .seismic
                .get()
                .total_cmp(&hazard_at(terrain, *b).seismic.get())
        });
        (cells[0], cells[cells.len() - 1])
    }

    /// A pure read, with no memory between calls — the same property
    /// [`hazard_at`] carries, restated for the stream because a draw is
    /// where statefulness would most plausibly creep in.
    #[test]
    fn events_in_is_a_pure_read() {
        let (geo, terrain) = globe();
        let (busiest, _) = extremes(&geo, &terrain);
        let first = events_in(Seed(42), &terrain, busiest, window());
        assert!(!first.is_empty(), "the busiest ground did nothing in 10 ky");
        for _ in 0..20 {
            assert_eq!(
                events_in(Seed(42), &terrain, busiest, window()),
                first,
                "the event stream carried state between calls"
            );
        }
    }

    /// The field drives the stream: ground with a shorter interval produces
    /// more events over the same window.
    ///
    /// Direction: this is the guard against a draw that ignores its
    /// recurrence — a fixed rate would leave every other test here green
    /// while the hazard field stopped reaching the events entirely.
    #[test]
    fn busier_ground_produces_more_events() {
        let (geo, terrain) = globe();
        let (busiest, quietest) = extremes(&geo, &terrain);
        let busy = events_in(Seed(42), &terrain, busiest, window()).len();
        let quiet = events_in(Seed(42), &terrain, quietest, window()).len();
        // The intervals differ by ~666x, so 10x is a loose band that a
        // Poisson fluctuation cannot cross.
        assert!(
            busy > 10 * quiet.max(1),
            "{busy} events on the busiest ground against {quiet} on the quietest"
        );
    }

    /// Events come back inside the window, in time order. The half-open
    /// convention is asserted at the top end: an event exactly at `end`
    /// belongs to the next window, not this one.
    #[test]
    fn events_lie_inside_the_window_and_are_ordered() {
        let (geo, terrain) = globe();
        let (busiest, _) = extremes(&geo, &terrain);
        let (start, end) = window();
        let events = events_in(Seed(42), &terrain, busiest, (start, end));
        assert!(!events.is_empty(), "no events to check");
        for pair in events.windows(2) {
            assert!(
                pair[0].day.day() <= pair[1].day.day(),
                "events came back out of order: {:?} then {:?}",
                pair[0],
                pair[1]
            );
        }
        for event in &events {
            assert!(
                event.day.day() >= start.day() && event.day.day() < end.day(),
                "{event:?} fell outside the window it was asked for"
            );
        }
    }

    /// An empty or reversed window is answered with nothing rather than with
    /// a panic or a full block's worth of events.
    #[test]
    fn an_empty_or_reversed_window_yields_nothing() {
        let (geo, terrain) = globe();
        let (busiest, _) = extremes(&geo, &terrain);
        let day = WorldTime::new(1_000.0).expect("finite");
        assert!(events_in(Seed(42), &terrain, busiest, (day, day)).is_empty());
        assert!(
            events_in(
                Seed(42),
                &terrain,
                busiest,
                (day, WorldTime::new(0.0).expect("finite"))
            )
            .is_empty()
        );
    }

    /// Both laws stay inside their authored brackets, and both kinds occur.
    #[test]
    fn magnitudes_lie_inside_their_authored_brackets() {
        let (geo, terrain) = globe();
        let long = (
            WorldTime::GENESIS,
            WorldTime::new(200_000.0 * Years::DAYS_PER_YEAR).expect("finite"),
        );
        let mut seismic = 0_u32;
        let mut eruptions = 0_u32;
        for cell in geo.cells() {
            if hazard_at(&terrain, cell).volcanic.is_none() && seismic > 1_000 {
                continue;
            }
            for event in events_in(Seed(42), &terrain, cell, long) {
                match event.kind {
                    HazardEventKind::Seismic => {
                        seismic += 1;
                        assert!(
                            (M_MIN..=M_MAX).contains(&event.magnitude),
                            "{event:?} escaped the authored magnitude bracket"
                        );
                    }
                    HazardEventKind::Eruption => {
                        eruptions += 1;
                        assert!(
                            (VEI_MIN..=VEI_MAX).contains(&event.magnitude),
                            "{event:?} escaped the authored VEI bracket"
                        );
                    }
                }
            }
        }
        assert!(seismic > 0, "no seismic event on the test globe");
        assert!(eruptions > 0, "no eruption on the test globe");
    }

    /// A cell with no edifice never erupts — the event-stream twin of
    /// [`volcanic_recurrence_exists_exactly_where_an_edifice_does`].
    #[test]
    fn an_amagmatic_cell_never_erupts() {
        let (geo, terrain) = globe();
        let long = (
            WorldTime::GENESIS,
            WorldTime::new(100_000.0 * Years::DAYS_PER_YEAR).expect("finite"),
        );
        let mut checked = 0_u32;
        for cell in geo.cells().take(400) {
            if has_edifice(&terrain, cell) {
                continue;
            }
            checked += 1;
            for event in events_in(Seed(42), &terrain, cell, long) {
                assert_eq!(
                    event.kind,
                    HazardEventKind::Seismic,
                    "{cell:?} has no edifice yet erupted: {event:?}"
                );
            }
        }
        assert!(checked > 0, "no amagmatic cell in the scan");
    }

    /// **One mountain, one eruption history.** Two different cells of one
    /// cone report the same eruptions, because the eruption process is keyed
    /// on the volcano's source contact rather than on the query cell.
    ///
    /// Direction: this is red the moment the eruption draw keys on `cell`,
    /// which would give the two halves of one mountain independent — and
    /// differently-timed — eruptions, the same defect Task 5 fixed for
    /// identity one level up. It says nothing about the seismic process,
    /// which is deliberately per-cell: a quake belongs to a belt, not to a
    /// named thing.
    ///
    /// Built at level 6 because a level-5 cone is usually a single cell, and
    /// a one-cell cone cannot exercise "different cells agree" at all — the
    /// same reason `volcano.rs`'s identity tests pay for level 6.
    #[test]
    fn every_cell_of_one_cone_shares_one_eruption_history() {
        let geo = Geosphere::new(6);
        let outcome = hornvale_terrain::generate(Seed(42), &geo, &TerrainPins::default())
            .expect("default pins generate");
        let terrain = GeneratedTerrain::new(geo.clone(), outcome);
        let long = (
            WorldTime::GENESIS,
            WorldTime::new(50_000.0 * Years::DAYS_PER_YEAR).expect("finite"),
        );
        let mut cones: std::collections::BTreeMap<CellId, Vec<CellId>> =
            std::collections::BTreeMap::new();
        for cell in geo.cells() {
            if let Some(source) = terrain.edifice_source_at(cell) {
                cones.entry(source).or_default().push(cell);
            }
        }
        let mut compared = 0_u32;
        for (source, cells) in cones.iter().filter(|(_, cells)| cells.len() > 1) {
            let eruptions = |cell: CellId| -> Vec<HazardEvent> {
                events_in(Seed(42), &terrain, cell, long)
                    .into_iter()
                    .filter(|e| e.kind == HazardEventKind::Eruption)
                    .collect()
            };
            let first = eruptions(cells[0]);
            assert!(
                !first.is_empty(),
                "the cone at {source:?} never erupted in 50 ky"
            );
            for cell in &cells[1..] {
                assert_eq!(
                    eruptions(*cell),
                    first,
                    "{cell:?} and {:?} are cells of the cone at {source:?} yet erupt on \
                     different days",
                    cells[0]
                );
                compared += 1;
            }
        }
        assert!(
            compared > 0,
            "no cone spans more than one cell — the property is untestable here"
        );
    }

    /// Every edifice cell on a globe, grouped by the source contact that
    /// identifies its cone.
    fn cones(geo: &Geosphere, terrain: &GeneratedTerrain) -> BTreeMap<CellId, Vec<CellId>> {
        let mut cones: BTreeMap<CellId, Vec<CellId>> = BTreeMap::new();
        for cell in geo.cells() {
            if let Some(source) = terrain.edifice_source_at(cell) {
                cones.entry(source).or_default().push(cell);
            }
        }
        cones
    }

    fn l6_globe(seed: u64) -> (Geosphere, GeneratedTerrain) {
        let geo = Geosphere::new(6);
        let outcome = hornvale_terrain::generate(Seed(seed), &geo, &TerrainPins::default())
            .expect("default pins generate");
        let terrain = GeneratedTerrain::new(geo.clone(), outcome);
        (geo, terrain)
    }

    /// **Where the field and the draw must agree, they do.** At an edifice's
    /// source contact, [`Recurrence::volcanic`] IS the rate `events_in` draws
    /// that mountain's eruptions at.
    ///
    /// This is the honest half of a divergence documented on
    /// [`Recurrence::volcanic`]: off-source the two differ by up to ~2.3x,
    /// because a mountain samples the field once at its source. At the source
    /// there is nothing to diverge, and pinning that keeps the relationship a
    /// stated one instead of a coincidence — a future `volcano_at` that drew
    /// its own recurrence, or scaled the field's, would make the field
    /// unrelated to the events everywhere rather than merely off-source.
    ///
    /// Direction: red if the volcano's recurrence stops being the field read
    /// at its source. It says nothing about off-source cells; its sibling
    /// below owns those, and the two were mutation-proved as a pair. Making
    /// `volcano_at` read the field at the QUERY cell leaves **this** test
    /// green — at a source, `cell` and `source` are the same cell, so no
    /// assertion here could ever see that mutation — and turns the sibling
    /// red on its non-vacuity guard. Neither test covers the other; the
    /// division is deliberate, and stating it is what stops a future reader
    /// from deleting one as redundant.
    #[test]
    fn an_edifice_sources_field_value_is_the_rate_its_eruptions_are_drawn_at() {
        let (geo, terrain) = l6_globe(42);
        let cones = cones(&geo, &terrain);
        assert!(!cones.is_empty(), "no edifice on the test globe");
        for source in cones.keys() {
            let field = hazard_at(&terrain, *source)
                .volcanic
                .expect("a source contact is an edifice cell");
            let drawn = crate::volcano::volcano_at(Seed(42), &terrain, *source)
                .expect("a source contact has a volcano")
                .recurrence;
            assert_eq!(
                field, drawn,
                "{source:?} is a source contact, yet its field value and the rate its \
                 eruptions are drawn at disagree"
            );
        }
    }

    /// **The off-source divergence is one-sided, and that is the known
    /// geometry.** A flank cell's local field value is never *more* active
    /// than the rate its mountain's eruptions are drawn at.
    ///
    /// Structural rather than lucky: within one cone every factor of `unrest`
    /// but proximity is the contact's own, proximity decays away from the
    /// boundary, and `geometric_years` is strictly decreasing in `unrest`. So
    /// the source — sitting on the boundary — is the most active cell of its
    /// cone, and every flank reads quieter. Measured over four globes on
    /// 2026-08-14: 499 disagreeing off-source cells, **zero** of them more
    /// active, worst ratio 2.2977.
    ///
    /// The non-vacuity assertion is the load-bearing half. Without it this
    /// test passes trivially against a `volcano_at` keyed on the query cell,
    /// where the two quantities are equal everywhere by construction — and
    /// that is precisely the design Task 5 exists to prevent.
    ///
    /// The worst ratio itself is deliberately NOT asserted: it is a property
    /// of terrain's unrest decay profile, not of this module, and pinning it
    /// here would make an unrelated terrain change look like a hazard defect.
    /// It is recorded on [`Recurrence::volcanic`] as measured data instead.
    ///
    /// claim: invariant(forall-seed) — a fixed, small seed set standing in for
    /// a structural property of the composition, in the shape
    /// `volcano.rs::an_edifices_source_is_itself_an_edifice` already uses.
    /// Not a rate and not a reachability claim, so not a census candidate.
    #[test]
    fn off_source_the_local_field_is_never_more_active_than_the_drawn_rate() {
        let mut disagreeing = 0_u32;
        let mut off_source = 0_u32;
        for seed in [42, 43, 44] {
            let (geo, terrain) = l6_globe(seed);
            for (source, cells) in &cones(&geo, &terrain) {
                for cell in cells.iter().filter(|c| *c != source) {
                    off_source += 1;
                    let local = hazard_at(&terrain, *cell)
                        .volcanic
                        .expect("an edifice cell")
                        .get();
                    let drawn = crate::volcano::volcano_at(Seed(seed), &terrain, *cell)
                        .expect("an edifice cell")
                        .recurrence
                        .get();
                    assert!(
                        local >= drawn,
                        "seed {seed}: {cell:?} is a flank of the cone at {source:?} yet its \
                         local field is MORE active than the rate its eruptions are drawn \
                         at: {local} y against {drawn} y"
                    );
                    disagreeing += u32::from(local != drawn);
                }
            }
        }
        assert!(
            off_source > 100,
            "only {off_source} off-source cells scanned"
        );
        // Non-vacuity: if the two quantities agreed everywhere there would be
        // no divergence to bound, and the inequality above would be free.
        assert!(
            disagreeing > 100,
            "only {disagreeing} off-source cells disagree with their drawn rate — the \
             divergence this test bounds has vanished, so the bound is vacuous"
        );
    }

    /// The key's spelling is a save-format contract, pinned here rather than
    /// discovered by a world that silently reseeds — the guard `volcano_key`
    /// and `chamber_key` both carry.
    #[test]
    fn the_event_key_spelling_is_pinned() {
        assert_eq!(
            event_key(CellId(0), HazardEventKind::Seismic, 0),
            "cell/0/process/seismic/block/0"
        );
        assert_eq!(
            event_key(CellId(4127), HazardEventKind::Eruption, -3),
            "cell/4127/process/eruption/block/-3"
        );
    }

    /// The block lattice tiles the whole timeline, before genesis included.
    #[test]
    fn the_block_lattice_covers_negative_days() {
        assert_eq!(block_index(0.0), 0);
        assert_eq!(block_index(BLOCK_DAYS - 1.0), 0);
        assert_eq!(block_index(BLOCK_DAYS), 1);
        assert_eq!(block_index(-1.0), -1);
        assert_eq!(block_index(-BLOCK_DAYS), -1);
        assert_eq!(block_index(-BLOCK_DAYS - 1.0), -2);
    }

    /// The count draw inverts the Poisson CDF: zero at `u = 0`, monotone
    /// non-decreasing in `u`, and never anything but zero at rate zero.
    #[test]
    fn the_block_count_inverts_the_poisson_cdf() {
        assert_eq!(poisson_count(0.0, 5.0), 0);
        assert_eq!(poisson_count(0.999, 0.0), 0);
        let mut previous = 0;
        for i in 0..=1000 {
            let count = poisson_count(f64::from(i) / 1001.0, 5.0);
            assert!(
                count >= previous,
                "the quantile went backwards at u={i}/1001"
            );
            previous = count;
        }
        // The median of Poisson(5) is 5, so the midpoint quantile must land
        // there — a check on the value and not only on the shape.
        assert_eq!(poisson_count(0.5, 5.0), 5);
    }

    /// The magnitude law is a **truncated** distribution and not a clamped
    /// one: it spans its bracket, it is strictly increasing in the uniform it
    /// consumes, and it approaches [`M_MAX`] without ever piling up on it.
    ///
    /// Direction: the discriminating assertion is the one at `u = 0.99999`.
    /// A clamped law — draw from the unbounded exponential, then `min(max)` —
    /// passes every other line here: it is monotone, it starts at `M_MIN`,
    /// and it reaches `M_MAX`. What it also does is put 10^-4.5 of its mass
    /// at exactly `M_MAX`, making the largest possible earthquake also the
    /// most common one above M 9. The truncated law is at 9.38 there; the
    /// clamped one is at 9.5 exactly.
    #[test]
    fn the_magnitude_law_is_truncated_and_not_clamped() {
        assert_eq!(truncated_magnitude(0.0, M_MIN, M_MAX, B_VALUE), M_MIN);
        let top = truncated_magnitude(1.0 - f64::EPSILON, M_MIN, M_MAX, B_VALUE);
        assert!(
            top <= M_MAX,
            "the law escaped its bracket at the top: {top}"
        );
        assert!(
            top > M_MAX - 0.001,
            "the law cannot reach the top of its bracket: {top}"
        );
        let near_top = truncated_magnitude(0.99999, M_MIN, M_MAX, B_VALUE);
        assert!(
            near_top < M_MAX - 0.01,
            "the law has a point mass at the top of its bracket: u=0.99999 gave {near_top}"
        );
        let mut previous = f64::NEG_INFINITY;
        for i in 0..=1000 {
            let m = truncated_magnitude(f64::from(i) / 1000.5, M_MIN, M_MAX, B_VALUE);
            assert!(m > previous, "the law is flat at u={i}/1000.5: {m}");
            previous = m;
        }
    }

    /// The window is half-open at **both** ends, `[start, end)`.
    ///
    /// A continuous draw never lands exactly on an arbitrary boundary, so
    /// this cannot be tested by asserting a bound on ordinary events — the
    /// probability is zero and the assertion would be vacuous. It is tested
    /// instead by taking a boundary FROM a drawn event: an event's own day
    /// used as `end` must exclude it, and used as `start` must include it.
    /// Direction: `day <= end` and `day > start` are each red here, and
    /// neither is caught by any other test in this file.
    #[test]
    fn the_window_is_half_open_at_both_ends() {
        let (geo, terrain) = globe();
        let (busiest, _) = extremes(&geo, &terrain);
        let (start, end) = window();
        let all = events_in(Seed(42), &terrain, busiest, (start, end));
        assert!(all.len() > 2, "too few events to pick an interior boundary");
        let pivot = all[all.len() / 2];
        let below = events_in(Seed(42), &terrain, busiest, (start, pivot.day));
        assert!(
            !below.contains(&pivot),
            "{pivot:?} was returned by a window that ENDS on its own day"
        );
        assert_eq!(
            below.len(),
            all.len() / 2,
            "the half-open end dropped or kept the wrong number of events"
        );
        let above = events_in(Seed(42), &terrain, busiest, (pivot.day, end));
        assert_eq!(
            above.first(),
            Some(&pivot),
            "a window that STARTS on an event's day did not return it"
        );
    }
}
