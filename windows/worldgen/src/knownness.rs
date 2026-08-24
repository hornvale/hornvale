//! Knownness (The Repose, spec §3.4): what a people still knows about the
//! mountain it lives on.
//!
//! # The flow balance `UNI-15` requires
//!
//! ```text
//!   SOURCE   an eruption occurs        ->  stock := 1   (the MOST RECENT one wins)
//!   DECAY    time passes               ->  stock *= decay(half-life)
//!   SINK     no living memory remains  ->  stock -> 0
//! ```
//!
//! **The parenthesis on SOURCE is a real rule and not a clarification.** Where
//! two eruptions share one horizon the stock decays from the later one and the
//! earlier contributes nothing — a people that has just watched its mountain
//! erupt knows it erupts, whatever it had forgotten the day before. That
//! sentence went un-asserted through the first round of this task while
//! reading as though it were obvious;
//! `the_most_recent_eruption_wins_over_an_earlier_one_in_the_same_horizon`
//! now holds it, and its doc records how a mutation worth 0.9989 of the unit
//! interval stayed green.
//!
//! **A fold over the event stream and nothing else.** No accumulation, no
//! state between calls, pure in its arguments — the same property
//! [`crate::hazard::events_in`] and [`crate::volcano::volcano_at`] carry, for
//! the same reason (`BIO-36`'s tier-0 rule and the Lorenz guard-rail). Asking
//! twice can only ever give the same answer, and asking about an earlier
//! `now` cannot be made cheaper by having asked about a later one.
//!
//! # It carries a holder, and it is allowed to be wrong
//!
//! Decision 0100 requires a holder of anything in the myth register: a stock
//! of knowledge is knowledge *someone has*, and an unowned one is a field
//! value wearing a memory's clothes. So [`Knownness`] names the people whose
//! memory it is, and the same mountain carries a different stock for each
//! people that can see it — the endonym/exonym shape
//! [`crate::volcano::volcano_name`] already uses, on deliberately the same
//! key.
//!
//! **It is explicitly permitted to contradict the hazard field, and that is
//! the campaign's point rather than a defect to engineer away.** The field
//! says how often the ground acts; the stock says what anybody remembers of
//! it acting. A mountain at the busy end of the authored bracket whose last
//! eruption fell outside living memory reads **zero** — Vesuvius in AD 79,
//! "not a known hazard; a fertile hill with towns on its flanks" (spec §1).
//! `a_dangerous_mountain_can_be_wholly_forgotten` holds that the
//! contradiction is representable; nothing here tries to prevent it.
//!
//! # Why this asks [`crate::hazard::events_in`] and never a recurrence
//!
//! The stock is folded over *events*, so it never needs a rate — and that
//! matters, because there are two rates here and they disagree.
//! [`crate::hazard::Recurrence::volcanic`] is the **local field value** at a
//! cell; the rate a mountain's eruptions are actually drawn at is
//! `volcano_at(seed, terrain, cell).recurrence`, sampled once at the
//! edifice's source contact. Off-source the two differ by up to ~2.3x on the
//! globes Task 6 measured, one-sidedly, with the local field always the
//! quieter. `events_in` draws at the mountain's rate, so folding its output
//! is right by construction where deriving anything from
//! `hazard_at(cell).volcanic` would have been quietly wrong on every flank
//! cell. `every_cell_of_one_cone_is_remembered_alike` is the assertion that
//! keeps it so.
//!
//! # No cross-species memory claim is preregistered here
//!
//! The half-life rides the holder's `generation_length`, so a longer-lived
//! people forgets more slowly. **When this campaign's spec was written that
//! coupling was inert** — `LifeSchedule::Paced` shipped with no occupant, so
//! lifespan was a pure function of mass and any "long-lived peoples remember
//! longer" prediction would really have been measuring mass. That is no
//! longer true: `Paced` now carries nine kinds (three dwarves at factor 4.0
//! from C2c, six elves at factor 5.0 from C2d), one of which — `drow` — is a
//! settling people this readout measures.
//!
//! **The decision not to predict stands; only its reason lapsed.** The spec
//! froze "no cross-species claim" *before* the code existed, and adding one
//! after discovering the axis went live is exactly the post-hoc move decision
//! 0016 exists to prevent — that it would now be a *better* prediction is
//! what makes it tempting and what makes it forbidden. The coupling is built
//! as specified, which is a change in what the code *means* and not in what
//! it does. Any cross-species spread the readout shows is reported as an
//! **observation** (spec §6.3 already requires per-people rows), never as a
//! tested prediction. A campaign that wants the question answered properly
//! now has a live axis and should preregister against it from a fresh spec.

use hornvale_kernel::{CellId, Seed, WorldTime, Years, math};
use hornvale_terrain::GeneratedTerrain;

use crate::hazard::HazardEventKind;

/// What one people still knows of one mountain, at one moment.
///
/// Never committed and never stored: C0 writes no facts, and the stock is
/// recomputed on demand from `(seed, terrain, holder, cell, now)` exactly as
/// the volcano and its events are (decision 0100's recompute test puts all
/// three in the phenomenon register).
/// type-audit: bare-ok(ratio: stock), bare-ok(identifier-text: holder)
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Knownness {
    /// How much is still known, in `[0, 1]`: 1 at an eruption, halving every
    /// [`memory_half_life`], and **exactly** 0 once the last eruption falls
    /// outside the horizon.
    ///
    /// The zero is a real sink and not an asymptote, so the value is either
    /// `0.0` or above `2^-MEMORY_HORIZON_HALF_LIVES` ≈ 0.00098 — see
    /// [`knownness`] for why the model needs a reachable sink and what the
    /// discontinuity costs.
    pub stock: f64,
    /// The people whose memory this is. Never `None`: a stock with no holder
    /// is a field value, which is the thing decision 0100 forbids in the myth
    /// register.
    pub holder: &'static str,
}

/// How many of the holder's generation lengths a memory survives before half
/// of it is gone.
///
/// AUTHORED at 2.0, and stated as a claim about transmission rather than
/// about biology: an event is still told by someone who *heard it from a
/// witness* two generations on, and by nobody at all much past that. One
/// generation would say a people forgets exactly as fast as its adults are
/// replaced, which ignores that they tell their children; three would put the
/// half-way point beyond any chain with a living witness at either end. Not
/// fitted to anything — no measurement in this campaign moves it, and
/// `the_half_life_rides_the_holders_generation_length` pins the arithmetic so
/// changing it is a deliberate act.
const MEMORY_GENERATIONS: f64 = 2.0;

/// How many half-lives the fold looks back before it declares the memory
/// gone.
///
/// AUTHORED at 10.0, and **this is the SINK**, not an optimisation. An
/// exponential decay never reaches zero, so a flow balance written with a
/// sink in it has to say where the sink is; truncating at ten half-lives says
/// it. The cost of the choice is exact and small: the stock discarded there is
/// `2^-10` ≈ 0.00098, under a tenth of a percent, so the stock is
/// discontinuous at the horizon by that much and by nothing more.
///
/// It also bounds the query, which matters for a second reason:
/// [`crate::hazard::events_in`]'s block loop is unbounded and a caller must
/// not hand it an unclamped span. The window here is ten half-lives wide —
/// on the shipped roster that is 314 years (`twig-blight`) to 2,819 years
/// (`drow`), one to three 1,000-year blocks.
///
/// **The bound is relative to the half-life, not absolute, and the scope of
/// that matters because [`knownness`] is `pub`.** It clamps the window
/// against the *present* — a caller cannot ask "everything since genesis" by
/// accident — but it cannot clamp against its own input: a caller passing a
/// generation length of `Years::new(1e12)` gets a window of 2e13 years and
/// hands `events_in` roughly 2e10 blocks, which will look hung exactly as
/// Task 6 warns. Nothing here defends against that, deliberately: a
/// generation length is resolved from the roster by
/// `generation_length_of`, and inventing a ceiling for a number that
/// physically cannot be large would be an unauthored constant guarding a
/// caller error. What is claimed is narrower than "structural": **the horizon
/// cannot be forgotten, only mis-supplied.**
const MEMORY_HORIZON_HALF_LIVES: f64 = 10.0;

/// The generation length assumed for a holder whose own cannot be derived.
///
/// AUTHORED, but not freely: it is the generation length of the life-history
/// allometry's **own calibration anchor** — a 40 kg endotherm on the ordinary
/// schedule, which `domains/species/src/allometry.rs` calibrates to a 60-year
/// lifespan and a 12-year maturity, giving `12 + 0.3 * (60 - 12) = 26.4`
/// years. Picking the anchor rather than a number means the fallback is the
/// same statement the rest of the roster is measured against instead of a
/// second, unrelated opinion about how long a generation is.
///
/// **It is reached only by an `Ametabolic` holder** — a construct, which has
/// no mass-derived life history at all (`life_history` nulls every biological
/// field for that class). No such kind founds a settlement on today's roster,
/// so this constant is exercised by
/// `a_holder_with_no_generation_length_falls_back_on_the_allometry_anchor`
/// and by nothing else; that test also fails the moment the anchor moves, so
/// the value cannot rot silently into disagreement with its own derivation.
///
/// The alternative — an unbounded stock for a holder that never dies — was
/// rejected: it makes the sink unreachable and asserts that constructs are
/// permanently right about their mountain, a claim this campaign has no basis
/// for.
const FALLBACK_GENERATION_YEARS: f64 = 26.4;

/// How long it takes a holder to forget half of what it knew.
///
/// [`MEMORY_GENERATIONS`] of the holder's own generation length, or of
/// [`FALLBACK_GENERATION_YEARS`] where that cannot be derived. The generation
/// length is the caller's to resolve and to pass — `generation_length_of` is
/// the published read for it — and a caller should resolve it **once per
/// people**, not once per query: that function re-assembles every canonical
/// registry on each call.
pub fn memory_half_life(generation_length: Option<Years>) -> Years {
    let generation = generation_length.map_or(FALLBACK_GENERATION_YEARS, |g| g.get());
    Years::new(MEMORY_GENERATIONS * generation)
        .expect("a generation length is finite and non-negative, and so is a multiple of one")
}

/// What `holder` still knows, at `now`, of the mountain the cell belongs to.
///
/// A fold over [`crate::hazard::events_in`]: the most recent eruption inside
/// the horizon sets the stock to 1 and it halves every [`memory_half_life`]
/// thereafter. Every earlier eruption is irrelevant by construction — a
/// people that has just watched its mountain erupt knows it erupts, whatever
/// it had forgotten the day before — so this is a fold with a `max`-like
/// combining rule and not an accumulation.
///
/// **`now` is exclusive.** The window is `[now - horizon, now)`, the same
/// half-open convention [`crate::hazard::events_in`] carries, so an eruption
/// happening at this very instant is not yet memory. The difference is one
/// instant and the convention is what makes two adjacent moments partition
/// their events with nothing counted twice.
///
/// **A cell with no edifice reads 0**, because there is no mountain to know
/// about. That case is answered before any draw — the early return is exactly
/// [`crate::volcano::volcano_at`]'s own gate and skips a seismic block draw
/// that could not have contributed an eruption anyway, so removing it would
/// change the cost and not the answer. A consumer aggregating over a
/// population is therefore measuring "how much of this population lives with
/// a remembered eruption", which is a share-like quantity over *everyone*,
/// not a mean over the few who have a mountain at all.
///
/// **Earthquakes are not a source.** They carry no identity — negating
/// "localized" yields a belt with no point of origin, and nobody names an
/// earthquake ([`crate::volcano`]'s module doc) — and you cannot forget a
/// thing you could never have named. The flow balance's SOURCE is an
/// eruption, so `HazardEventKind::Seismic` is filtered out here rather than
/// silently folded in.
///
/// `holder` and `generation_length` must describe the **same** people; the
/// pair is the caller's to keep honest, because resolving it here would
/// re-assemble the canonical registries on every call.
/// type-audit: bare-ok(identifier-text: holder)
pub fn knownness(
    seed: Seed,
    terrain: &GeneratedTerrain,
    holder: &'static str,
    generation_length: Option<Years>,
    cell: CellId,
    now: WorldTime,
) -> Knownness {
    if crate::volcano::volcano_at(seed, terrain, cell).is_none() {
        return Knownness { stock: 0.0, holder };
    }
    let half_life_days = memory_half_life(generation_length).days();
    let horizon_days = MEMORY_HORIZON_HALF_LIVES * half_life_days;
    let start = WorldTime::from_std_days(now.as_std_days() - horizon_days)
        .expect("a finite horizon before a finite present is a finite day");
    // `.rev()` IS THE SOURCE RULE, not a style choice: `events_in` returns
    // time-ordered events, so reversing before `find` takes the MOST RECENT
    // eruption in the horizon rather than the earliest. Deleting it type-
    // checks, keeps every other test in this file green, and moves the stock
    // by up to 0.9989 wherever two eruptions share one horizon.
    // `the_most_recent_eruption_wins_over_an_earlier_one_in_the_same_horizon`
    // is the only assertion that objects.
    let last = crate::hazard::events_in(seed, terrain, cell, (start, now))
        .into_iter()
        .rev()
        .find(|e| e.kind == HazardEventKind::Eruption);
    let stock = match last {
        // `half_life_days` is strictly positive: `Years::new` rejects
        // negatives and the only zero it could take is a zero generation
        // length, which the allometry cannot produce (maturity is positive
        // for any positive mass).
        Some(event) => math::powf(
            0.5,
            (now.as_std_days() - event.day.as_std_days()) / half_life_days,
        ),
        None => 0.0,
    };
    Knownness { stock, holder }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hazard::{HazardEvent, hazard_at};
    use hornvale_kernel::Geosphere;
    use hornvale_terrain::TerrainPins;
    use std::collections::BTreeMap;

    /// The mesh level these tests build at. Level 6 for the same reason
    /// `volcano.rs` pays for it: a level-5 cone is usually a single cell, and
    /// `every_cell_of_one_cone_is_remembered_alike` cannot be exercised at
    /// all by a one-cell cone.
    const LEVEL: u32 = 6;

    /// A test holder's generation length: 20 years, so the half-life is 40
    /// and the horizon 400 — both round numbers a reader can check by hand
    /// against the assertions below, and neither taken from the roster (a
    /// roster change must not silently move what these tests mean).
    const TEST_GENERATION_YEARS: f64 = 20.0;

    fn test_generation() -> Option<Years> {
        Some(Years::new(TEST_GENERATION_YEARS).expect("a positive span"))
    }

    fn half_life_days() -> f64 {
        memory_half_life(test_generation()).days()
    }

    /// The horizon these tests expect, **spelled out in years rather than
    /// derived from [`MEMORY_HORIZON_HALF_LIVES`]**: 20-year generations give
    /// a 40-year half-life, and ten of those is 400 years.
    ///
    /// The spelling-out is the point, and it was learned from a mutation.
    /// This helper originally read `MEMORY_HORIZON_HALF_LIVES *
    /// half_life_days()`, which moves *with* the constant — so raising the
    /// horizon from 10 half-lives to 20 left the whole file GREEN, and the
    /// constant that decides where the SINK sits was pinned by nothing at
    /// all. A test that derives its expectation from the value under test
    /// asserts only that the code agrees with itself.
    const TEST_HORIZON_YEARS: f64 = 400.0;

    fn horizon_days() -> f64 {
        TEST_HORIZON_YEARS * Years::DAYS_PER_YEAR
    }

    fn globe_of(seed: u64) -> (Geosphere, GeneratedTerrain) {
        let geo = Geosphere::new(LEVEL);
        let outcome = hornvale_terrain::generate(Seed(seed), &geo, &TerrainPins::default())
            .expect("default pins generate");
        let terrain = GeneratedTerrain::new(geo.clone(), outcome);
        (geo, terrain)
    }

    fn globe() -> (Geosphere, GeneratedTerrain) {
        globe_of(42)
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

    /// A day, as a [`WorldTime`].
    fn at(day: f64) -> WorldTime {
        WorldTime::from_std_days(day).expect("a finite day")
    }

    /// This cell's eruptions over a long span of world time, in order.
    fn eruptions(seed: u64, terrain: &GeneratedTerrain, cell: CellId) -> Vec<HazardEvent> {
        let window = (WorldTime::GENESIS, at(200_000.0 * Years::DAYS_PER_YEAR));
        crate::hazard::events_in(Seed(seed), terrain, cell, window)
            .into_iter()
            .filter(|e| e.kind == HazardEventKind::Eruption)
            .collect()
    }

    /// The first `(cell, eruption, gap_to_the_next)` on the globe whose gap is
    /// at least `min_gap_days` — the shape both the decay and the sink tests
    /// need, since an intervening eruption would reset the stock they are
    /// following.
    fn eruption_with_quiet_after(
        seed: u64,
        geo: &Geosphere,
        terrain: &GeneratedTerrain,
        min_gap_days: f64,
    ) -> (CellId, HazardEvent, f64) {
        for source in cones(geo, terrain).keys() {
            let events = eruptions(seed, terrain, *source);
            for pair in events.windows(2) {
                let gap = pair[1].day.as_std_days() - pair[0].day.as_std_days();
                if gap >= min_gap_days {
                    return (*source, pair[0], gap);
                }
            }
        }
        panic!("no eruption on the globe is followed by {min_gap_days} quiet days");
    }

    /// The first `(cell, earlier, later)` on the globe where two CONSECUTIVE
    /// eruptions both fall inside one horizon measured from just after the
    /// later one — the only shape that can tell "most recent wins" from "any
    /// eruption in the horizon wins".
    ///
    /// The gap is bracketed on both sides and each bound is load-bearing.
    /// **Above one half-life**, so the two candidate answers are far apart
    /// (decaying from the earlier one has already lost at least half its
    /// stock) and a test comparing them cannot pass on a rounding accident.
    /// **Below 90% of the horizon**, so that with `now` set just after the
    /// later eruption the earlier one is still genuinely INSIDE the window —
    /// if it fell outside, the fold could not have picked it whatever its
    /// order, and the test would be vacuous for a second reason.
    fn two_eruptions_inside_one_horizon(
        seed: u64,
        geo: &Geosphere,
        terrain: &GeneratedTerrain,
    ) -> (CellId, HazardEvent, HazardEvent) {
        let (lower, upper) = (half_life_days(), 0.9 * horizon_days());
        for source in cones(geo, terrain).keys() {
            let events = eruptions(seed, terrain, *source);
            for pair in events.windows(2) {
                let gap = pair[1].day.as_std_days() - pair[0].day.as_std_days();
                if gap > lower && gap < upper {
                    return (*source, pair[0], pair[1]);
                }
            }
        }
        panic!(
            "no cone on the globe has two consecutive eruptions between {lower} and {upper} \
             days apart — the most-recent-wins property is untestable here"
        );
    }

    /// **SOURCE then DECAY.** The stock is 1 immediately after an eruption
    /// and exactly half of it one half-life later.
    ///
    /// Direction: the `0.5` assertion is the load-bearing one — it pins that
    /// the half-life IS the half-life, and it is red against any decay law
    /// whose rate is not [`memory_half_life`], including a stock welded to 1
    /// and a stock that decays on some other constant. The `> 0.999`
    /// assertion alone would not be: it is satisfied by a stock that never
    /// decays at all.
    ///
    /// The window is chosen with a quiet gap after the eruption, because a
    /// second eruption inside it would legitimately reset the stock to 1 and
    /// the test would be reading a different event than the one it selected.
    #[test]
    fn knownness_peaks_at_an_eruption_and_decays_after_it() {
        let (geo, terrain) = globe();
        let (cell, event, _) =
            eruption_with_quiet_after(42, &geo, &terrain, half_life_days() * 1.5);
        let just_after = knownness(
            Seed(42),
            &terrain,
            "aeldrin",
            test_generation(),
            cell,
            at(event.day.as_std_days() + 1.0),
        );
        assert!(
            just_after.stock > 0.999,
            "the day after an eruption the stock reads {} — the SOURCE does not set it to 1",
            just_after.stock
        );
        let one_half_life = knownness(
            Seed(42),
            &terrain,
            "aeldrin",
            test_generation(),
            cell,
            at(event.day.as_std_days() + half_life_days()),
        );
        assert!(
            (one_half_life.stock - 0.5).abs() < 1e-9,
            "one half-life after an eruption the stock reads {} and not 0.5 — the DECAY \
             does not run on the holder's half-life",
            one_half_life.stock
        );
        assert!(
            one_half_life.stock < just_after.stock,
            "the stock did not fall between {} and {}",
            just_after.stock,
            one_half_life.stock
        );
    }

    /// **SOURCE, the half the other tests could not see: the MOST RECENT
    /// eruption wins.** With two eruptions inside one horizon, the stock
    /// decays from the LATER one, and the earlier one contributes nothing.
    ///
    /// **This property was pinned by NOTHING until this test existed**, and
    /// the gap is worth recording rather than quietly closing. Deleting
    /// `.rev()` from the fold — making it take the EARLIEST eruption in the
    /// horizon instead of the latest — left all eleven of this file's tests
    /// green, while moving the answer by up to 0.9989 on 4,644 (cell, now)
    /// samples of seed 42 alone. It hid because every other test here selects
    /// an eruption with a quiet gap AFTER it and never constrains the window
    /// BEFORE it, and because the 400-year test horizon is usually shorter
    /// than the 200–5,000 year eruption intervals, so a second eruption
    /// rarely landed inside it by chance. The committed fixture did catch the
    /// mutation, but that is a gate-full byte golden with no named meaning:
    /// the commit-gate battery was blind, and the module doc's load-bearing
    /// sentence — "every earlier eruption is irrelevant by construction" —
    /// was asserted by nothing.
    ///
    /// Direction: red under any fold that picks a different eruption from the
    /// horizon's set — earliest, largest, first-found. The two `assert`s are
    /// a pair: the first says the answer IS the later eruption's decay, the
    /// second says the earlier eruption's decay is a materially different
    /// number, so the first cannot be satisfied by accident.
    #[test]
    fn the_most_recent_eruption_wins_over_an_earlier_one_in_the_same_horizon() {
        let (geo, terrain) = globe();
        let (cell, earlier, later) = two_eruptions_inside_one_horizon(42, &geo, &terrain);
        let now = at(later.day.as_std_days() + 1.0);

        // Non-vacuity, asserted rather than argued: BOTH eruptions must lie
        // inside the window the fold actually reads, or the later one wins
        // for the trivial reason that the earlier one was never a candidate.
        let elapsed_since_earlier = now.as_std_days() - earlier.day.as_std_days();
        assert!(
            elapsed_since_earlier < horizon_days(),
            "the earlier eruption is {elapsed_since_earlier} days back, outside the \
             {} day horizon — it was never a candidate and this test proves nothing",
            horizon_days()
        );

        let stock = knownness(Seed(42), &terrain, "aeldrin", test_generation(), cell, now).stock;
        let from_later = math::powf(
            0.5,
            (now.as_std_days() - later.day.as_std_days()) / half_life_days(),
        );
        let from_earlier = math::powf(0.5, elapsed_since_earlier / half_life_days());
        assert!(
            (stock - from_later).abs() < 1e-9,
            "the stock reads {stock}, which is not the decay from the LATER eruption \
             ({from_later}); the decay from the earlier one is {from_earlier}"
        );
        assert!(
            (from_later - from_earlier).abs() > 0.4,
            "the two candidate answers are only {} apart ({from_later} against \
             {from_earlier}) — the assertion above cannot distinguish them",
            (from_later - from_earlier).abs()
        );
    }

    /// **SINK.** Past the horizon with no new eruption the stock is exactly
    /// zero, and one day inside it is not.
    ///
    /// Direction: the pair is the point. The zero alone is satisfied by a
    /// stock that is always zero; the positive reading one day inside the
    /// horizon is what makes the boundary a boundary rather than a floor the
    /// implementation happens to sit under.
    ///
    /// **The horizon is spelled out at 400 years and not derived from
    /// [`MEMORY_HORIZON_HALF_LIVES`]** — see [`TEST_HORIZON_YEARS`]. Deriving
    /// it left the constant unpinned: moving the sink from ten half-lives to
    /// twenty was GREEN across this whole file. The upper bound on `inside`
    /// pins the depth from the other side, since it is the cost the constant's
    /// own doc claims for the truncation.
    #[test]
    fn knownness_decays_to_nothing_when_the_mountain_is_quiet() {
        let (geo, terrain) = globe();
        let horizon = horizon_days();
        let (cell, event, gap) = eruption_with_quiet_after(42, &geo, &terrain, horizon + 2.0);
        assert!(
            gap > horizon,
            "the selected gap {gap} does not exceed the horizon {horizon}"
        );
        let inside = knownness(
            Seed(42),
            &terrain,
            "aeldrin",
            test_generation(),
            cell,
            at(event.day.as_std_days() + horizon - 1.0),
        );
        assert!(
            inside.stock > 0.0,
            "one day inside the horizon the stock is already zero — the sink fires early"
        );
        assert!(
            inside.stock < 0.002,
            "one day inside the horizon the stock still reads {} — the sink is deeper than \
             the ten half-lives it is authored at, and discards more than the tenth of a \
             percent that constant's doc claims",
            inside.stock
        );
        let outside = knownness(
            Seed(42),
            &terrain,
            "aeldrin",
            test_generation(),
            cell,
            at(event.day.as_std_days() + horizon + 1.0),
        );
        assert_eq!(
            outside.stock, 0.0,
            "one day past the horizon the stock is {} and not zero — there is no SINK, \
             only an asymptote",
            outside.stock
        );
    }

    /// **The stock is allowed to contradict the hazard field, and this shows
    /// the contradiction is REPRESENTABLE.** A mountain the field calls
    /// dangerous can read wholly forgotten.
    ///
    /// Direction: it asserts the case exists on a real globe, never that any
    /// particular world exhibits it at any particular moment. It goes red
    /// against an implementation that clamps the stock away from zero, that
    /// derives it from the recurrence instead of from events, or that
    /// otherwise refuses to let the two disagree — which is the failure this
    /// campaign is most at risk of engineering in by accident.
    ///
    /// "Dangerous" is the busy half of the authored bracket: under 1,000
    /// years between eruptions, against a quiet end of 5,000. The non-vacuity
    /// assertion is that such mountains exist at all on the globe.
    #[test]
    fn a_dangerous_mountain_can_be_wholly_forgotten() {
        let (geo, terrain) = globe();
        // Only a sampling range here, never an expectation — this test
        // asserts that a forgotten dangerous mountain EXISTS, not where the
        // horizon is. [`TEST_HORIZON_YEARS`] carries the pin.
        let horizon = horizon_days();
        let mut dangerous = 0_u32;
        let mut forgotten = 0_u32;
        for source in cones(&geo, &terrain).keys() {
            let volcano = crate::volcano::volcano_at(Seed(42), &terrain, *source)
                .expect("a source contact has a volcano");
            if volcano.recurrence.get() >= 1_000.0 {
                continue;
            }
            dangerous += 1;
            // Sample a century apart across ten horizons — enough moments
            // that a mountain with a gap longer than the horizon anywhere in
            // that span is found, and cheap because each query reads at most
            // a handful of 1,000-year blocks.
            let mut day = 0.0;
            while day < 10.0 * horizon {
                let stock = knownness(
                    Seed(42),
                    &terrain,
                    "aeldrin",
                    test_generation(),
                    *source,
                    at(day),
                )
                .stock;
                if stock == 0.0 {
                    forgotten += 1;
                    break;
                }
                day += 100.0 * Years::DAYS_PER_YEAR;
            }
        }
        assert!(
            dangerous > 0,
            "no mountain on the globe is in the busy half of the authored bracket — \
             the contradiction is untestable here"
        );
        assert!(
            forgotten > 0,
            "{dangerous} dangerous mountains and not one of them is ever forgotten — the \
             stock cannot contradict the field, which is the property spec §3.4 requires"
        );
    }

    /// A cell with no edifice has nothing to remember, so the stock is zero
    /// there whatever the ground has been doing seismically.
    ///
    /// Direction: red against a fold that treats an earthquake as a source.
    #[test]
    fn a_cell_with_no_edifice_has_nothing_to_remember() {
        let (geo, terrain) = globe();
        let mut checked = 0_u32;
        for cell in geo.cells().take(2_000) {
            if crate::hazard::has_edifice(&terrain, cell) {
                continue;
            }
            checked += 1;
            let known = knownness(
                Seed(42),
                &terrain,
                "aeldrin",
                test_generation(),
                cell,
                at(5_000.0 * Years::DAYS_PER_YEAR),
            );
            assert_eq!(
                known.stock, 0.0,
                "{cell:?} carries no edifice yet something is remembered of its mountain"
            );
        }
        assert!(checked > 0, "no amagmatic cell in the scan");
    }

    /// **The coupling.** The half-life is [`MEMORY_GENERATIONS`] of the
    /// holder's generation length, so a longer-lived people forgets more
    /// slowly.
    ///
    /// Direction: the two exact half-life assertions are red against any
    /// hard-coded half-life and against a changed [`MEMORY_GENERATIONS`]; the
    /// stock comparison is red against a half-life that is computed but not
    /// used. This asserts the mechanism only — **no cross-species prediction
    /// is made here or anywhere in this campaign**, for the reason the module
    /// doc gives.
    #[test]
    fn the_half_life_rides_the_holders_generation_length() {
        let short = Years::new(20.0).expect("a positive span");
        let long = Years::new(100.0).expect("a positive span");
        assert_eq!(memory_half_life(Some(short)).get(), 40.0);
        assert_eq!(memory_half_life(Some(long)).get(), 200.0);

        let (geo, terrain) = globe();
        let (cell, event, _) = eruption_with_quiet_after(42, &geo, &terrain, horizon_days());
        let now = at(event.day.as_std_days() + memory_half_life(Some(short)).days());
        let quick = knownness(Seed(42), &terrain, "aeldrin", Some(short), cell, now);
        let slow = knownness(Seed(42), &terrain, "khorrun", Some(long), cell, now);
        assert!(
            slow.stock > quick.stock,
            "the longer-generation holder does not remember more: {} against {}",
            slow.stock,
            quick.stock
        );
    }

    /// A holder with no derivable generation length falls back on the
    /// allometry's own calibration anchor.
    ///
    /// Direction: the second assertion is the ratchet. It re-derives 26.4
    /// years from the shipped law rather than restating it, so a change to
    /// `domains/species`'s anchor turns this red and forces
    /// [`FALLBACK_GENERATION_YEARS`] to be re-authored instead of rotting
    /// into silent disagreement with the thing it claims to be.
    #[test]
    fn a_holder_with_no_generation_length_falls_back_on_the_allometry_anchor() {
        assert_eq!(
            memory_half_life(None).get(),
            MEMORY_GENERATIONS * FALLBACK_GENERATION_YEARS
        );
        let anchor = hornvale_species::life_history(
            hornvale_kernel::Mass::new(40.0).expect("a positive mass"),
            hornvale_species::MetabolicClass::Endotherm,
            hornvale_species::LifeSchedule::Allometric,
        )
        .generation_length
        .expect("an endotherm has a generation length");
        assert!(
            (anchor.get() - FALLBACK_GENERATION_YEARS).abs() < 1e-9,
            "the allometry's 40 kg endotherm anchor now generates every {} years, not \
             {FALLBACK_GENERATION_YEARS} — re-author the fallback against the new anchor",
            anchor.get()
        );
    }

    /// Pure, with no memory between calls — the property every read in this
    /// campaign carries, restated here because a fold is where statefulness
    /// would most plausibly creep in.
    #[test]
    fn knownness_is_pure_and_carries_no_state() {
        let (geo, terrain) = globe();
        let (cell, event, _) = eruption_with_quiet_after(42, &geo, &terrain, half_life_days());
        let now = at(event.day.as_std_days() + 1.0);
        let first = knownness(Seed(42), &terrain, "aeldrin", test_generation(), cell, now);
        assert!(
            first.stock > 0.0,
            "nothing remembered — the check is vacuous"
        );
        for _ in 0..20 {
            assert_eq!(
                knownness(Seed(42), &terrain, "aeldrin", test_generation(), cell, now),
                first,
                "the stock carried state between calls"
            );
        }
    }

    /// The stock names its holder, and two peoples asking about one mountain
    /// get two separately-held answers rather than one shared field value
    /// (decision 0100).
    #[test]
    fn the_stock_names_the_people_that_holds_it() {
        let (geo, terrain) = globe();
        let source = *cones(&geo, &terrain)
            .keys()
            .next()
            .expect("a volcano on the test globe");
        let now = at(5_000.0 * Years::DAYS_PER_YEAR);
        for people in ["aeldrin", "khorrun"] {
            assert_eq!(
                knownness(Seed(42), &terrain, people, test_generation(), source, now).holder,
                people,
                "the stock reported some other people's memory as {people}'s"
            );
        }
    }

    /// **One mountain, one memory.** Two different cells of one cone are
    /// remembered identically by one people, because the eruptions being
    /// folded are the mountain's and not the cell's.
    ///
    /// Direction: red the moment the fold reads a per-cell eruption history —
    /// the Task 6 trap one level up. A cone's flank reads a *quieter* local
    /// volcanic field than its source (one-sided, up to ~2.3x), so a stock
    /// derived from `hazard_at(cell).volcanic` rather than folded over
    /// `events_in` would give the two halves of one mountain different
    /// memories of the same eruption, and nothing else in this file would
    /// notice.
    #[test]
    fn every_cell_of_one_cone_is_remembered_alike() {
        let (geo, terrain) = globe();
        let now = at(8_000.0 * Years::DAYS_PER_YEAR);
        let all = cones(&geo, &terrain);
        let multi = all.values().filter(|cells| cells.len() > 1).count();
        assert!(
            multi > 0,
            "no cone spans more than one cell — the agreement property is untestable here"
        );
        let mut compared = 0_u32;
        let mut remembered = 0_u32;
        for (source, cells) in all.iter().filter(|(_, cells)| cells.len() > 1) {
            let first = knownness(
                Seed(42),
                &terrain,
                "aeldrin",
                test_generation(),
                cells[0],
                now,
            );
            remembered += u32::from(first.stock > 0.0);
            for cell in &cells[1..] {
                assert_eq!(
                    knownness(Seed(42), &terrain, "aeldrin", test_generation(), *cell, now),
                    first,
                    "{cell:?} and {:?} are cells of the cone at {source:?} yet their people \
                     remembers them differently",
                    cells[0]
                );
                compared += 1;
            }
        }
        assert!(compared > 0, "no multi-cell cone compared");
        // Non-vacuity: agreement on a globe where every stock is zero would
        // be free. At least one cone must actually be remembered.
        assert!(
            remembered > 0,
            "{multi} multi-cell cones and not one of them is remembered at all — the \
             agreement asserted above is agreement on nothing"
        );
    }

    /// The stock never leaves `[0, 1]`, over a whole globe's mountains and a
    /// span of moments.
    ///
    /// claim: invariant(forall-seed) — a fixed, small seed set standing in for
    /// a structural property of the decay law, in the shape
    /// `hazard.rs::off_source_the_local_field_is_never_more_active_than_the_drawn_rate`
    /// already uses. Not a rate and not a reachability claim, so not a census
    /// candidate.
    #[test]
    fn the_stock_never_leaves_the_unit_interval() {
        let mut sampled = 0_u32;
        let mut positive = 0_u32;
        for seed in [42, 43] {
            let (geo, terrain) = globe_of(seed);
            for source in cones(&geo, &terrain).keys() {
                for step in 0..8 {
                    let now = at(f64::from(step) * 700.0 * Years::DAYS_PER_YEAR);
                    let stock = knownness(
                        Seed(seed),
                        &terrain,
                        "aeldrin",
                        test_generation(),
                        *source,
                        now,
                    )
                    .stock;
                    sampled += 1;
                    positive += u32::from(stock > 0.0);
                    assert!(
                        (0.0..=1.0).contains(&stock),
                        "seed {seed}: {source:?} at day {} reads a stock of {stock}",
                        now.as_std_days()
                    );
                }
            }
        }
        assert!(sampled > 1_000, "only {sampled} samples taken");
        assert!(
            positive > 0,
            "every one of {sampled} samples read zero — the bound is vacuous"
        );
    }

    /// The field and the stock are different quantities, and the globe shows
    /// them disagreeing in both directions at once: mountains the field calls
    /// quiet that are freshly remembered, and mountains it calls busy that
    /// are wholly forgotten.
    ///
    /// Direction: red against any implementation in which the stock is a
    /// monotone function of the recurrence — which is what a stock derived
    /// from the field rather than folded over events would be.
    #[test]
    fn the_stock_is_not_a_function_of_the_hazard_field() {
        let (geo, terrain) = globe();
        let now = at(12_000.0 * Years::DAYS_PER_YEAR);
        let mut quiet_but_known = 0_u32;
        let mut busy_but_forgotten = 0_u32;
        for source in cones(&geo, &terrain).keys() {
            let field = hazard_at(&terrain, *source)
                .volcanic
                .expect("a source contact is an edifice cell")
                .get();
            let stock = knownness(
                Seed(42),
                &terrain,
                "aeldrin",
                test_generation(),
                *source,
                now,
            )
            .stock;
            if field >= 2_500.0 && stock > 0.5 {
                quiet_but_known += 1;
            }
            if field < 1_000.0 && stock == 0.0 {
                busy_but_forgotten += 1;
            }
        }
        assert!(
            quiet_but_known > 0 && busy_but_forgotten > 0,
            "the stock tracks the field: {quiet_but_known} quiet-but-remembered and \
             {busy_but_forgotten} busy-but-forgotten mountains at one moment"
        );
    }
}
