//! The Tithe (living-community C3 slice 2 — tribute): the committed
//! measurement gates, and the campaign's headline adjudication.
//!
//! The Tumult built predation and measured it deeply sub-critical. Its
//! diagnosis was that the model had **dissipation but no accumulation**. This
//! campaign supplied accumulation — a store, a standing tribute relation, a
//! negotiation under mutual ignorance, a strategy family indexed by the
//! patron's time horizon (spec §4.3a), and finally a failure mode (revolt)
//! once patrons could be wounded rather than only killed (§4.3e). This file
//! holds spec §8's criteria as assertions, and reports the §5 headline on the
//! same instrument The Tumult used (`cascade_sizes` via `history_for`, pooled
//! over seeds 1..=30, replicated over 1..=100).
//!
//! # DISCLOSURE — read this before the numbers (spec §5 requires it)
//!
//! **This campaign amended its own spec five times, four of them following a
//! disappointing measurement.** In order: (1) the adaptive-demand rule was
//! corrected before implementation because the first formulation was a
//! one-signed ratchet that could not oscillate; (2) hysteresis was added to
//! the patronage transfer after ~87% churn was measured; (3) the
//! milk-never-kill cap was replaced by a bleed after the health signal was
//! shown unable to go negative; (4) extraction strategy was made a
//! discount-rate family after `assessment / eff_capacity` was measured at
//! **exactly one value across all 2258 relations**; (5) relation continuity
//! across relocation was added after revolt was measured to fire **zero**
//! times in thirty worlds.
//!
//! That cumulative shape is metric-chasing, whatever each local
//! justification. It was named to the project owner mid-campaign and he chose
//! to proceed; the protection taken was to **preregister** amendment 5's
//! predictions in spec §4.3e *before any of its code existed*, with both
//! branches made informative in advance. A reader who sees only the final
//! histogram below, without knowing how many mechanisms were added chasing
//! it, has been misled. No constant was tuned toward either verdict at any
//! point (§4.3e prediction 3, confirmed: the only `const`s in amendment 5's
//! diff are inside a test fixture), and every floor in this file is set clear
//! of its measured value rather than at it.
//!
//! # PRIMARY VERDICT (spec §5): the branching ratio roughly DOUBLED and the
//! # distribution is still geometric. Self-organized criticality is FALSIFIED
//! # a second time.
//!
//! Pooled over `SHAPE_SAMPLE` (seeds 1..=30) at HEAD: `hist [634, 36, 0×10]`
//! — 634 cascades of size 1, 36 of size 2–3, and **nothing at all above size
//! 3**, out of 7183 conquests. Replicated over seeds 1..=100:
//! `hist [1889, 98, 0×10]` out of 22 255 conquests — the same shape at 3.0×
//! the sample.
//!
//! **These counts are post-absorption.** They were first measured on the
//! campaign's own tree, where the same run gave `hist [346, 21]` / σ
//! 0.095–0.101 (1..=30). Absorbing main brought The Vacancy's *fifth Settled
//! people*, the gnoll — which, at `time_horizon` 0.2, is the shortest-sighted
//! patron in the strategy family and so the most extractive one. More peoples
//! means more communities, more conquests and far more flight (83 pooled
//! flights became 998), and every count in this file roughly doubled with the
//! map's occupancy. **The verdict below is unchanged in every particular**;
//! only the magnitudes moved, and they moved together.
//!
//! The **branching ratio** — secondary displacements per top-level conquest,
//! the σ whose critical value is 1 — is computed exactly as The Tumult
//! computed it, and is reported as an interval because the histogram is
//! log-binned (bin 1 spans sizes 2–3, so the secondary count is bounded, not
//! known):
//!
//! ```text
//! secondaries S  =  Σ over bins of count_i × [2^i, 2^(i+1) − 1]
//! conquests   P  =  census.raided − S      (`raided` counts BOTH a top-level
//!                                           raid and a cascade's evictions)
//! σ              =  S / P
//! ```
//!
//! | arm | sample | hist | raided | S | P | **σ** |
//! |---|---|---|---|---|---|---|
//! | The Tumult | 1..=30 | `[38, 2]` | 886 | 42–44 | 842–844 | **0.050–0.052** |
//! | The Tumult | 1..=100 | `[138, 3]` | 2974 | 144–147 | 2827–2830 | **0.051–0.052** |
//! | **The Tithe** | 1..=30 | `[634, 36]` | 7183 | 706–742 | 6441–6477 | **0.109–0.115** |
//! | **The Tithe** | 1..=100 | `[1889, 98]` | 22255 | 2085–2183 | 20072–20170 | **0.103–0.109** |
//!
//! **So the preregistered question has a two-part answer, and both parts
//! ship.** Accumulation *did* move the branching ratio off σ ≈ 0.051: it
//! roughly doubled, to σ ≈ 0.106, and it moved by the same factor on both
//! samples. But σ ≈ 0.1 is not σ ≈ 1. The distribution's *shape* did not
//! change at all:
//!
//! - **Occupied support is still bins 0–1.** The largest cascade in 100
//!   worlds is 3 displacements — **0.48 decades** of support, against the
//!   ≥ ~1.5 decades spec §5 requires to call a power law.
//! - **The per-octave drop did not soften into a tail.** Bin 0 → bin 1 falls
//!   **17.6×** at 1..=30 and **19.3×** at 1..=100, where a heavy tail would
//!   fall by `2^(1−τ)` ≈ 2–4×. (The Tumult's drop was 19× and 46×; the
//!   softening is the same factor-of-two the σ shows, not a change of
//!   family.)
//! - **Bin 2 and above are empty on every one of the 100 worlds**, i.e. not
//!   one cascade past 3 displacements in ~22 000 conquests. A geometric law
//!   with σ ≈ 0.1 predicts exactly that; a power law does not.
//!
//! This is therefore **spec §5's documented falsification, for the second
//! time and at a doubled branching ratio** — a geometric distribution with a
//! hard cutoff, deep in the sub-critical regime. It ships as the campaign's
//! answer, exactly as §4.3e prediction 2 said in advance that it would if the
//! shape held while revolts fired (they do fire: 29 pooled revolts and 998
//! flights at 1..=30, 72 and 3054 at 1..=100, where the pre-continuity build
//! measured **0** revolts on the same thirty worlds).
//!
//! **The diagnosis this leaves is structural, and both remaining levers were
//! deferred on purpose (§9).** A revolt frees exactly one vassal
//! (collapse-release is a non-goal), and the relation graph is a set of
//! **one-level stars** (depth is a non-goal), so a patron's failure has no
//! medium along which to propagate. An avalanche needs a medium and slice 2
//! has none. Accumulation alone is not enough; **depth (chaining) or release
//! is the next lever**, and this measurement is the evidence for promoting
//! one of them.
//!
//! # SECONDARY VERDICT (spec §5, adjudicated SEPARATELY): no secular cycle.
//!
//! Reported on its own axis and deliberately **not** bundled into the primary
//! verdict. Measured on the **per-relation** tribute series, never on raw
//! volume — raw volume tracks the relation count and would report the
//! population's shape rather than the demand's (spec §5 says so, and the
//! campaign learned it the hard way).
//!
//! (Also measured pre-absorption, and on the same reverted probe as §8.0's
//! table; the shape claim below is what ships, not the counts.) Over 754
//! relations holding ≥ 20 contiguous collections (pooled 1..=30),
//! the dominant Fourier component of the detrended per-relation remittance
//! series sits at harmonic index **k = 1 in 91.6% of relations** (86.2% for
//! the extraction-rate series, 93–95% at k ≤ 2). k = 1 *is* the series' own
//! length — the lowest resolvable frequency — so what the periodogram is
//! finding is **drift, not oscillation**. No period shorter than a relation's
//! whole life is resolvable in it. The world-level mean-extraction-rate
//! series behaves identically: on 26 of 30 seeds the peak sits at k = 1 or
//! k = 2 of a ~77-epoch span, and the autocorrelation shows no repeating
//! peak (first ACF maximum absent or below 0.25 on all but one seed).
//!
//! The peak carries more power than a white-noise shuffle (median 0.264 vs
//! 0.101) but that is what *any* smooth series does; against an AR(1)
//! red-noise null fitted to each relation's own lag-1 correlation the excess
//! is small (0.264 vs 0.164) and it is concentrated at k = 1, i.e. in the
//! trend rather than in a cycle.
//!
//! **A mechanism-level reading, and it is not a surprise.** §4.3's feedback
//! was made two-signed precisely so it *could* cycle, but amendment 4 then
//! gave it a **setpoint** (§4.3a): the patron steers its vassal toward a
//! target stock fixed by its own horizon, and the health signal became the
//! controller that *reaches* that target rather than a loop hunting around
//! whatever happened. A well-damped regulator converges; it does not
//! oscillate. The Ibn Khaldun / Turchin secular cycle §4.3 hoped to reproduce
//! as a consequence is **not present in this build**, and the honest reading
//! is that amendment 4 suppressed the very oscillation amendment 3 had made
//! possible.
//!
//! # §8.0 — the variety criterion. MET, and it is what this campaign can
//! # claim unambiguously.
//!
//! **Measured PRE-ABSORPTION, on the campaign's own tree.** This section's
//! numbers came from a temporary probe that was reverted before commit, so
//! they cannot be cheaply re-measured after absorbing main; they are left as
//! the dated record they are. The absorption roughly doubled every population
//! count in this file (see the primary verdict above), so read the ratios and
//! the monotonicity — which is what §8.0 actually claims — rather than the
//! absolute `n`s. The gate below, which *is* re-measured, still passes.
//!
//! Measured over 5401 relations pooled across seeds 1..=30 (a temporary
//! env-gated probe, inert — seed 42's census with the probe is identical to
//! without — and reverted before commit; the numbers are recorded in the
//! campaign's T6 report). The pre-amendment state had `assessment /
//! eff_capacity` at **exactly one value** across all 2258 relations; the
//! shipped rule produces **3173 distinct relation-level extraction rates**
//! over 4740 relations that ever collect, and 41 635 distinct per-collection
//! rates over 52 230 collections.
//!
//! | patron people | `time_horizon` | n | extraction rate (median) | relation lifetime (median) | extinction |
//! |---|---|---|---|---|---|
//! | bugbear | 0.3 | 2032 | 0.1053 | 100 yr | **0.79%** |
//! | hobgoblin | 0.5 | 1843 | 0.0577 | 150 yr | 0.27% |
//! | kobold | 0.8 | 1526 | 0.0342 | 175 yr | **0.20%** |
//!
//! Monotone in the authored horizon on every axis, with a 3.1× spread in
//! extraction rate. **Extinction is the exception and it concentrates on the
//! short horizon**, which is §8.0's own red line: 24 of 5401 relations
//! (**0.44%**) end with the vassal's line ending — 8 to famine, 16 lost on
//! the road after being driven off — and a bugbear patron's vassal is **4×**
//! likelier to be one of them than a kobold patron's. That is Clark's case
//! arriving as a consequence of the discount model rather than as a special
//! rule (§4.3a), which is exactly what the criterion asks for.
//!
//! The `the_strategy_family_is_various` gate below binds the *observable
//! projection* of this — standing-relation lifetime by patron horizon, which
//! `History` carries without any probe. It is survivorship-biased by
//! construction (only relations alive at `now` are in it) and it is stated as
//! such; the fate and extraction-rate columns above are probe measurements,
//! reported rather than gated.
//!
//! *Seed-42 readings, dated 2026-07-28 (post-absorption) rather than standing
//! claims (only the `const` floors below are asserted):* `grew 9583, founded
//! 395, migrated 308, raided 591, fled 591, collapsed 83, resettled 533,
//! records_total 1776, alive_at_now 329, cascade_hist [62, 2, 0×10],
//! subordinations_formed 635, patronage_transfers 46,
//! tribute_relations_at_now 159, max_subordinates 5, tribute_collected
//! 7349.093, tribute_collection_events 4487, max_stores_at_now 181.996,
//! vassal_flights 68, vassal_revolts 1`.
//!
//! *The same readings on the campaign's own tree, dated 2026-07-27, for the
//! record:* `grew 9375, founded 378, migrated 175, raided 455, fled 455,
//! collapsed 49, resettled 390, records_total 1405, alive_at_now 344,
//! cascade_hist [30, 8, 0×10], subordinations_formed 495,
//! patronage_transfers 30, tribute_relations_at_now 164, max_subordinates 5,
//! tribute_collected 8002.397, tribute_collection_events 4555,
//! max_stores_at_now 249.052, vassal_flights 8, vassal_revolts 5`.
//!
//! ## THIS PANEL BECAME A CENSUS COLUMN (The Assize, 2026-08-08)
//!
//! The twelve-seed panel this section used to hold
//! (`the_tribute_accumulator_is_reported_over_a_panel`, `FLOOR_PANEL`,
//! `MIN_TRIBUTE_COLLECTED`) is deleted. Three things are worth recording about
//! the replacement:
//!
//! 1. **It became `tribute-relations-standing`** (`windows/lab/src/metrics.rs`).
//! 2. **The census cannot reach the flow this panel read.**
//!    `BakeCensus::tribute_collected` lives on `History::tally`, which
//!    `build_world_to` discards before any census view exists — no metric can
//!    read it. `tribute-relations-standing` instead measures the
//!    `pays-tribute-to` **stock** the ledger keeps. It agrees with the
//!    discarded flow at spearman **0.934** over 36 worlds, which is a measured
//!    witness that the two move together, **not** a claim that they are the
//!    same quantity — one is a flow and the other a stock.
//! 3. **The retired panel was already near-vacuous.** Every candidate
//!    observable tried, including the flow itself, measured **0/36 zeros**
//!    over the probe worlds — so this panel's own `live * 2 >= n`
//!    non-inertness guard could essentially never fire; there was no
//!    plausible world on which it would have caught anything.

use hornvale_astronomy::SkyPins;
use hornvale_kernel::{EntityId, KindId, Seed, Value, World};
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{
    BakeCensus, History, SettlementPins, SkyChoice, WorldComponents, cascade_sizes, census,
    emit_history, history_for, register_all,
};
use std::collections::BTreeMap;

/// The seed sample the pooled verdicts are measured over — the same fixed
/// range The Tumult pooled its shape verdict over, so the two campaigns are
/// directly comparable. The finding replicates unchanged at 1..=100 (module
/// docs), which is reported rather than run here: 100 live bakes is a
/// campaign-scale measurement, not a gate.
const SHAPE_SAMPLE: std::ops::RangeInclusive<u64> = 1..=30;

/// Spec §8.1 — the subordination floor on seed 42. Measured **495** first-time
/// subordinations (takeovers counted separately, 30, and deliberately not
/// pooled in: churn between rival patrons must not be readable as volume).
/// Pinned at 100, ~5× clear. A run below it means the subordination branch
/// went inert, which §8.1 makes a `RAID_MARGIN`/horizon calibration finding
/// for the owner and never a floor to lower.
const MIN_SUBORDINATIONS: u64 = 100;

/// Spec §8.2 — the accumulator floor: the largest store any **alive**
/// community still holds at `now`. Seed 42 measures **249.052**; pinned at
/// 40, ~6× clear. A stock standing at `now` is the load-bearing half of "the
/// structure accumulates": stores decay each epoch and die with their holder,
/// so a large one says a patron collected *and survived collecting*, which is
/// the thing predation could not do.
const MIN_MAX_STORES: f64 = 40.0;

/// Spec §8.3 — the not-depopulated floor, on the Tithe's own build. Seed 42
/// measures **344** records alive at `now`; pinned at 100, ~3.4× clear.
/// Deliberately tighter than `history_tumult.rs`'s 50, because extraction is
/// a population *sink* (every unit remitted leaves a population and enters a
/// store that never re-enters growth) and this campaign has already measured
/// one build that thinned the world by 62%.
const MIN_ALIVE_AT_NOW: u64 = 100;

/// Spec §8.1/§8.3 — the standing-stock floor: relations still in force at
/// `now`, where `MIN_SUBORDINATIONS` above is a flow. Seed 42 measures
/// **164**; pinned at 40, ~4× clear. Flow and stock are floored separately
/// because a build with enormous churn and no surviving relation would clear
/// the flow floor while accumulating nothing.
const MIN_STANDING_RELATIONS: usize = 40;

/// Spec §8.3 — the floor that says extraction does not consume its own tax
/// base. `records_total` is every occupation ever opened over the whole span,
/// so it measures how much *living* the world did, where `alive_at_now`
/// measures only what was left standing at the end. Seed 42 measures **1405**;
/// pinned at 400, ~3.5× clear.
///
/// **This is the reading that catches farming-to-death, and it was chosen by
/// measurement rather than by argument.** The mid-campaign investigation found
/// that the pre-amendment-4 build's collapse was not a fertility effect at all
/// — the per-eligible-epoch founding rate barely moved — but a *community-epoch*
/// effect: 100 180 → 57 122 over 24 seeds, because farmed vassals died young
/// and their sites stopped producing daughters. `records_total` is the direct
/// measure of that, and the mutation arm confirms it: a patron that strips its
/// vassal to `FARM_FLOOR` every epoch takes seed 42 from 1405 records to 228.
///
/// **How far this floor actually reaches, measured rather than argued (T6's
/// mutation ladder).** The arm above is severe — it deletes the assessment and
/// the concealment term together and takes every unit above the floor — and it
/// is the only one of five tried that reddens this test. The milder arms all
/// stay green, and the reason is worth knowing before anyone reads this floor
/// as a general guard on over-extraction:
///
/// **This ladder was measured PRE-ABSORPTION** (its HEAD row is seed 42's
/// then-current 344/1405; the merged tree reads 329/1776). Re-running five
/// mutation arms is campaign-scale work, so it is left as the dated record it
/// is — the finding it carries is the *ordering* of the arms, which no
/// population shift disturbs.
///
/// | mutation | `alive_at_now` | `records_total` |
/// |---|---|---|
/// | (none — HEAD, pre-absorption) | 344 | 1405 |
/// | `Bake::target_stock` returns `0.0` (no setpoint, no floor) | 136 | 1407 |
/// | the setpoint pinned to `FARM_FLOOR` for every patron | 150 | 1450 |
/// | the assessment cap removed (take everything above the setpoint) | 247 | **2024** |
/// | `bleed` measured on `population` (reaches through the floor) | 346 | 1276 |
/// | strip to `FARM_FLOOR` unconditionally, no cap, no concealment | **48** | **228** |
///
/// Two readings follow, and both are mechanism findings rather than test
/// bookkeeping. First, **the assessment cap — not `FARM_FLOOR` — is the
/// first-order bound on how fast a vassal can be drained**: with the cap intact
/// the setpoint floor can be deleted outright and the world still ends with 136
/// standing. Second, and sharper: **`records_total` moves the WRONG WAY under
/// moderate over-extraction** — 1405 → 2024 when the cap is removed. A daughter
/// needs `pressure < DAUGHTER_MAX_PRESSURE` and nothing else, so a bled vassal
/// is the *least* crowded community in the world and founds *more* daughters,
/// not fewer. That is the same trap the rejected famine-share instrument fell
/// into, one level up, and it means this floor catches farming-to-death only in
/// the regime where extraction is severe enough to stop the world generating
/// settlement at all. It is not a monotone dial on cruelty and must not be read
/// as one.
///
/// **The instrument this replaced does NOT work, and the negative result is
/// recorded here so nobody reaches for it again.** The obvious gate is "the
/// share of occupations ending in `CauseOfEnd::Famine`" — but `Famine` fires
/// on `pressure >= COLLAPSE_PRESSURE`, i.e. on *overcrowding*, and a vassal
/// farmed to the floor is the least crowded community in the world. Under the
/// ruthless-extraction mutation seed 42's famine share falls from 1.6% to
/// 0.4%: the ceiling would have moved the **wrong way** and passed greener the
/// worse the world got. That is a narrated assertion, not a measured one.
const MIN_RECORDS_TOTAL: u64 = 400;

/// Spec §5 — the pooled cascade floor, on the phenomenon EXISTING rather than
/// on its shape. `SHAPE_SAMPLE` pools **670** cascades; pinned at 100. No
/// ceiling is asserted on the shape anywhere in this file: the shape is the
/// falsification (module docs), and pinning against it would freeze the very
/// result the deferred depth/release levers are meant to break.
const MIN_POOLED_CASCADES: u64 = 100;

/// Spec §4.3d — the flight floor. `SHAPE_SAMPLE` pools **998** flights; pinned
/// at 20. Floored because a still-geometric cascade distribution is only
/// interpretable if the mechanisms that were supposed to move it actually
/// fired: a null with an inert mechanism is a different (and much weaker)
/// finding than a null with a live one.
const MIN_POOLED_FLIGHTS: u64 = 20;

/// Spec §4.3d/§4.3e — the revolt floor, and the campaign's preregistered
/// prediction 1 made permanent. `SHAPE_SAMPLE` pools **29** revolts where the
/// pre-continuity build measured **0** across the same thirty worlds; pinned
/// at 5. This is the assertion that stops the headline from silently decaying
/// into "the mechanism did not fire".
const MIN_POOLED_REVOLTS: u64 = 5;

/// Spec §8.0 — the variety floor: how many standing relations a patron people
/// must hold before its lifetime median is read at all. Measured minimum was
/// **484** (bugbear), over the three patron peoples the roster had *when that
/// reading was taken*; pinned at 100.
const MIN_RELATIONS_PER_PEOPLE: usize = 100;

/// Spec §8.0 — **the retired two-point margin, kept as a printed readout and
/// asserted on nothing.** Pooled median standing-relation lifetime measured
/// **325** standard days for the longest-horizon patron people (kobold, 0.8)
/// against **175** for the shortest sampled at the time (bugbear, 0.3), a
/// ratio of **1.857**; the criterion required 1.30.
///
/// **This number is no longer a bar, because the quantity it read is not a
/// property of the world.** The assertion it drove compared `rows[0]` against
/// `rows[last]` — two order statistics off a roster of nine — and The
/// Underworld measured it flipping verdict without the relationship moving at
/// all. On `main` at `95cbaa70` the top row was *desert-dwarf* (horizon 0.90,
/// 116 relations, median 400 d) against gnoll's 350 d: ratio **1.143**, green.
/// On the same campaign's tree the top row was *high-elf* (horizon 0.95, 196
/// relations, median 675 d) against gnoll's 475 d: ratio **1.421**, red. The
/// two peoples at the top **are not the same people**: desert-dwarf fell below
/// [`MIN_RELATIONS_PER_PEOPLE`] and high-elf rose above it, so the verdict
/// turned on which people crossed the inclusion threshold, not on how lifetime
/// tracks horizon. Meanwhile the whole-roster rank correlation moved only
/// 0.384 → 0.546, both inside the noise bar below.
///
/// See [`NOISE_Z`] for what replaced it and
/// [`the_strategy_family_is_various`] for the derivation.
const RETIRED_ENDPOINT_RATIO_BAR: f64 = 1.30;

/// Spec §8.0 — **the whole-roster tripwire's constant**, and the only thing
/// this file asserts about the horizon/lifetime relationship.
///
/// The two-sided 5% standard-normal quantile. Under the null of no
/// association, Spearman's rho over `n` points has mean 0 and standard
/// deviation ≈ `1/sqrt(n - 1)`, so `NOISE_Z / sqrt(n - 1)` is the roster's own
/// 5% critical value and it re-derives itself whenever the roster grows or
/// shrinks. At the nine patron peoples measured here that is **0.6930**, which
/// agrees with the tabulated exact value for n = 9 (0.683–0.700) to within the
/// table's own spread; at n = 15 the formula gives 0.524 against a tabulated
/// 0.521.
///
/// Chosen this way deliberately: a hard-coded bar would be a claim with a date
/// the moment a people entered or left the roster — which is *exactly* the
/// failure the retired endpoint ratio above just demonstrated.
const NOISE_Z: f64 = 1.96;

/// Build seed 42's history through the standalone measurement entry point —
/// the same assembly `history_tumult.rs` uses, so the two files' readings are
/// the same instrument.
fn history(seed: u64) -> History {
    let wc = WorldComponents::assemble().expect("registries");
    history_for(
        Seed(seed),
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
        &wc,
    )
    .expect("bakes")
}

/// The number of secondary displacements the log-binned cascade histogram is
/// consistent with, as an inclusive `(low, high)` interval. Bin `i` counts
/// cascades whose size lies in `[2^i, 2^(i+1) − 1]`, so a bin of count `n`
/// contributes between `n × 2^i` and `n × (2^(i+1) − 1)` displacements — the
/// size is bounded by the binning, never known. Reported as an interval for
/// exactly that reason, as The Tumult reported it.
fn secondaries(hist: &[u64; 12]) -> (u64, u64) {
    let mut lo = 0;
    let mut hi = 0;
    for (i, &n) in hist.iter().enumerate() {
        lo += n * (1u64 << i);
        hi += n * ((1u64 << (i + 1)) - 1);
    }
    (lo, hi)
}

/// Spec §8.1 — **subordination fires, at volume.** The gate that says branch 2
/// of the raid rule is new motive rather than a relabelling of the shipped
/// covet gate: seed 42 forms hundreds of standing relations on targets that
/// keep their cell, their people and their life, none of which the eviction
/// branch could have produced. Takeovers are excluded, so churn between rival
/// patrons cannot be read as volume (§4.4's hysteresis note).
#[test]
fn subordination_fires_at_volume() {
    let c: BakeCensus = census(&history(42));
    eprintln!("TITHE seed-42 census: {c:?}");
    assert!(
        c.subordinations_formed >= MIN_SUBORDINATIONS,
        "subordination inert: only {} first-time relations formed on seed 42 (floor \
         {MIN_SUBORDINATIONS}, transfers {}) — branch 2 of the raid rule is not firing; a \
         calibration finding for the owner, not a floor to lower.",
        c.subordinations_formed,
        c.patronage_transfers
    );
}

/// Spec §8.2 — **the structure accumulates, and its holder survives holding
/// it.** Both halves are floored, because either alone is satisfiable by the
/// other going to zero: a flow with no surviving stock is an extractor that
/// dies of its own success, and a stock with no flow is impossible.
///
/// The mechanism claim §8.2 states — a patron's `stores` rise while its
/// `pressure` does **not**, so a successful extractor never starves itself —
/// is bound on a fixture at the unit level, where `stores` and `pressure` are
/// reachable (`a_patron_accumulates_stores_without_its_pressure_rising` and
/// `stores_raise_strength_but_never_pressure` in `history_bake.rs`). This gate
/// binds its *world-scale consequence*: on a live world, a store survives to
/// `now` in a community that neither starved nor was conquered. `stores` decay
/// every epoch and are destroyed with their holder, so a large standing stock
/// cannot be reached by a patron that was starving.
///
/// `max_subordinates` is **reported and not floored**: cardinality is
/// deliberately unbounded (§4.4) and a runaway hub is a finding, not a failure.
///
/// # The volume half moved to a census column (The Delvers, then The Assize)
///
/// This test used to assert `tribute_collected >= MIN_TRIBUTE_COLLECTED` here,
/// on seed 42 alone. It no longer does, and the floor was **not lowered** —
/// it simply no longer exists as a constant. What this test asserts about
/// volume is now only **non-inertness**, which is what its own failure
/// message always said it was for ("the accumulator is inert"). The
/// calibrated reading moved (The Delvers, 2026-08-07) to a twelve-seed panel,
/// and then (The Assize, 2026-08-08) to the census column
/// `tribute-relations-standing` — see this file's module doc for why that
/// column measures a stock rather than this flow.
///
/// **Seed 42 is one sample of a distribution three orders of magnitude
/// wide** (7.055 to 6936.212 across ordinary seeds), and it was a *low*
/// sample at 1450.482 against a twelve-world panel median of 3663.131. A
/// scalar floor read on one world cannot see that spread, so it cannot
/// distinguish "the branch died" from "this world is small". The census is
/// the real instrument now; this test is the cheap tripwire that keeps the
/// branch's liveness in the commit gate.
///
/// The stock half (`max_stores_at_now` against [`MIN_MAX_STORES`]) is
/// untouched and still asserted here.
#[test]
fn the_structure_accumulates_without_starving_its_holder() {
    let c = census(&history(42));
    eprintln!(
        "TITHE seed-42 accumulator: collected {:.3} over {} events, max store at now {:.3}, \
         widest star {} subordinates (seed 42 is ONE sample of a 3-order-of-magnitude \
         distribution — the census column `tribute-relations-standing` is the instrument)",
        c.tribute_collected, c.tribute_collection_events, c.max_stores_at_now, c.max_subordinates
    );
    assert!(
        c.tribute_collected > 0.0 && c.tribute_collection_events > 0,
        "the accumulator is INERT on seed 42: {:.3} remitted over {} collection events. This \
         is not a calibration reading — it is the branch having stopped running at all. The \
         calibrated volume reading lives in the census; this one only says the mechanism \
         fires.",
        c.tribute_collected,
        c.tribute_collection_events
    );
    assert!(
        c.max_stores_at_now >= MIN_MAX_STORES,
        "nothing accumulated that survived: the largest store standing at now is {:.3} (floor \
         {MIN_MAX_STORES}) though {:.3} was collected — the extractors are dying with their \
         hoards, which is dissipation wearing accumulation's clothes",
        c.max_stores_at_now,
        c.tribute_collected
    );
}

/// Spec §8.3 — **the map is not depopulated, and no community is farmed to
/// extinction by tribute alone.** Two readings, because tribute can empty a
/// world two ways: by leaving too few standing at the end (`alive_at_now`),
/// and by killing communities so young that the world stops generating them at
/// all (`records_total`). The two are independent — a world can churn a great
/// many short lives and end thinly, or open few and keep them — so both are
/// floored.
///
/// Tribute is a population **sink**: every unit remitted leaves a population
/// and enters a store that never re-enters growth or pressure. That is why
/// these floors are tighter than `history_tumult.rs`'s, and it is the effect
/// that thinned the pre-amendment-4 build by 62%.
///
/// The mechanism claim beneath this — that a subordinate may be bled *toward*
/// `FARM_FLOOR` and never *through* it (§4.2b) — is bound on a fixture at the
/// unit level, where a single relation's population is reachable
/// (`no_subordinate_is_farmed_below_the_farm_floor_by_tribute` in
/// `history_bake.rs`). This gate binds the world-scale consequence.
#[test]
fn extraction_does_not_depopulate_the_map() {
    let h = history(42);
    let c = census(&h);
    eprintln!(
        "TITHE seed-42 depopulation check: alive {} of {} records opened, collapsed {}",
        c.alive_at_now, c.records_total, c.collapsed
    );
    assert!(
        c.alive_at_now >= MIN_ALIVE_AT_NOW,
        "extraction depopulated the world: only {} records alive at now (floor \
         {MIN_ALIVE_AT_NOW}, collapsed {})",
        c.alive_at_now,
        c.collapsed
    );
    assert!(
        c.records_total >= MIN_RECORDS_TOTAL,
        "extraction consumed its own tax base: only {} occupations were ever opened over the \
         whole span (floor {MIN_RECORDS_TOTAL}, collapsed {}) — farmed vassals dying young stop \
         the world generating settlement, which is how the pre-amendment build lost 62% of it \
         (spec §4.2b/§8.3)",
        c.records_total,
        c.collapsed
    );
}

/// Spec §4.4 — **dissolution is a coherence floor**, checked on a live world
/// rather than on a fixture. A relation may not outlive either party in either
/// role: `tribute` holds community handles, so an entry naming a closed
/// community is the silent corruption §4.4 exists to forbid, and it would
/// surface as a wrong collection on some unrelated seed rather than here.
///
/// The relation count is floored in the same test so the invariant cannot pass
/// vacuously on an empty table — a coherence check over zero relations is the
/// green-and-unreddenable shape this campaign has already been bitten by.
#[test]
fn every_standing_relation_names_two_living_communities() {
    let h = history(42);
    let alive: BTreeMap<_, _> = h
        .records
        .iter()
        .map(|r| (r.community, r.core.is_alive()))
        .collect();
    assert!(
        h.tribute.len() >= MIN_STANDING_RELATIONS,
        "only {} relations stand at now (floor {MIN_STANDING_RELATIONS}) — the coherence check \
         below would pass vacuously",
        h.tribute.len()
    );
    for t in &h.tribute {
        assert_eq!(
            alive.get(&t.subordinate),
            Some(&true),
            "a standing relation names a subordinate that is not alive at now: {t:?}"
        );
        assert_eq!(
            alive.get(&t.patron),
            Some(&true),
            "a standing relation names a patron that is not alive at now: {t:?}"
        );
        assert_ne!(
            t.subordinate, t.patron,
            "a community pays tribute to itself: {t:?}"
        );
    }
}

/// **No emitted `pays-tribute-to` fact may be dated before either entity it
/// names existed** — the chronological floor under the ledger, read off the
/// REAL seed-42 ledger rather than off a fixture (final review, Important 1).
///
/// `Fact.day` means "the day this became true" (`kernel/src/ledger.rs`), so a
/// relation fact dated before one of its two parties was founded is a claim no
/// timeline can render. Twenty-two of seed 42's 164 tribute facts were exactly
/// that, by up to **675 years**: `Bake::carry_portfolio_to` preserved a
/// relation's `since` while the patron became a new community with a new
/// `EntityId`, so the obligation continued (correctly) but the fact naming the
/// new lord kept the old lord's start date. It was role-asymmetric — 0 of 164
/// on the subordinate side — exactly as spec §4.3e's asymmetry predicts, since
/// a fleeing vassal drops its relation and only the patron side survives a
/// move.
///
/// This is checked on the **emitted ledger** and not on `History::tribute`,
/// because the ledger is what a chronicle or timeline reader consumes and
/// `emit_history` is the code that stamps the day. Emitting into a bare
/// registered `World` costs one bake and no terrain, so the check sits in the
/// commit gate rather than the heavy tier.
///
/// Anti-vacuity is asserted from both ends: the fact count is floored (a check
/// over zero facts is the green-and-unreddenable shape this campaign has
/// already been bitten by twice), and every fact must resolve BOTH parties to a
/// founding day, so a lookup that silently missed would fail rather than skip.
#[test]
fn no_emitted_tribute_fact_predates_either_party() {
    let h = history(42);
    let mut world = World::new(Seed(42));
    register_all(&mut world.registry).expect("the registry accepts every domain's concepts");
    emit_history(&mut world, &h).expect("the seed-42 history commits");

    // Every occupation's founding day, off the same ledger — never off
    // `h.records`, because what is under test is what the LEDGER says, and a
    // check that read one side from the bake could agree with itself while the
    // emitted pair disagreed.
    let founded: BTreeMap<_, _> = world
        .ledger
        .iter()
        .filter(|f| f.predicate == hornvale_history::OCC_FOUNDED)
        .map(|f| match f.object {
            Value::Number(day) => (f.subject, day),
            ref other => panic!("occ-founded must carry a number, got {other:?}"),
        })
        .collect();

    let relations: Vec<(EntityId, EntityId, f64)> = world
        .ledger
        .iter()
        .filter(|f| f.predicate == hornvale_history::PAYS_TRIBUTE_TO)
        .map(|f| match f.object {
            Value::Entity(patron) => (
                f.subject,
                patron,
                f.day.expect("a dated relation fact").as_std_days(),
            ),
            ref other => panic!("pays-tribute-to must carry an entity, got {other:?}"),
        })
        .collect();
    assert!(
        relations.len() >= MIN_STANDING_RELATIONS,
        "only {} tribute facts were emitted (floor {MIN_STANDING_RELATIONS}) — the date \
         check below would pass vacuously",
        relations.len()
    );

    let mut impossible = Vec::new();
    for (subordinate, patron, day) in &relations {
        let sub_founded = *founded
            .get(subordinate)
            .expect("every tribute subject is an emitted occupation with a founding day");
        let patron_founded = *founded
            .get(patron)
            .expect("every tribute object is an emitted occupation with a founding day");
        if *day < sub_founded || *day < patron_founded {
            impossible.push((*subordinate, *patron, *day, sub_founded, patron_founded));
        }
    }
    assert!(
        impossible.is_empty(),
        "{} of {} `pays-tribute-to` facts on seed 42 are dated before an entity they name \
         existed. A relation may outlast a patron's community — the obligation continues — \
         but the FACT names the community that holds it now, and it may not predate it. \
         (subordinate, patron, fact day, subordinate founded, patron founded): {:?}",
        impossible.len(),
        relations.len(),
        &impossible[..impossible.len().min(8)]
    );
}

/// **THE HEADLINE (spec §5), heavy.** Pool `cascade_sizes` over `SHAPE_SAMPLE`
/// on the same instrument The Tumult used and adjudicate the shape, printing
/// the branching ratio, the per-octave decay and the decades of support so the
/// two campaigns are read off one table.
///
/// **The verdict is recorded in the module docs and it is a falsification:**
/// σ roughly doubled (0.051 → ~0.106) and the distribution stayed geometric
/// with a hard cutoff. Per measure-don't-narrate the only things asserted here
/// are floors saying the phenomena EXIST — cascades, flights, revolts — never
/// a ceiling on the shape. A ceiling would freeze the falsification that the
/// deferred depth/release levers are meant to break.
/// claim: readout(off-gate, heavy:) — cascade-size distribution over
/// SHAPE_SAMPLE, adjudicated
#[test]
#[ignore = "heavy: live-worldgen battery; deferred from the commit gate to the heavy set (decision 0132)"]
fn the_cascade_distribution_is_adjudicated() {
    let mut agg = [0u64; 12];
    let mut raided = 0u64;
    let mut resettled = 0u64;
    let mut alive = 0u64;
    let mut flights = 0u64;
    let mut revolts = 0u64;
    for s in SHAPE_SAMPLE {
        let h = history(s);
        let c = census(&h);
        let hi = cascade_sizes(&h);
        eprintln!(
            "TITHE seed {s}: raided {} resettled {} alive {} flights {} revolts {} \
             relations {} hist {hi:?}",
            c.raided,
            c.resettled,
            c.alive_at_now,
            c.vassal_flights,
            c.vassal_revolts,
            c.tribute_relations_at_now
        );
        raided += c.raided;
        resettled += c.resettled;
        alive += c.alive_at_now;
        flights += c.vassal_flights;
        revolts += c.vassal_revolts;
        for (a, b) in agg.iter_mut().zip(hi.iter()) {
            *a += b;
        }
    }
    let cascades: u64 = agg.iter().sum();
    let (slo, shi) = secondaries(&agg);
    // `raided` counts a cascade's evictions as well as top-level conquests
    // (both increment it), so the number of relaxations a raid STARTED is the
    // difference — the same denominator The Tumult reported σ against.
    let (plo, phi) = (raided - shi, raided - slo);
    let largest = agg.iter().rposition(|&n| n > 0).unwrap_or(0);
    eprintln!(
        "TITHE pooled: hist {agg:?} cascades {cascades} raided {raided} resettled {resettled} \
         alive {alive} flights {flights} revolts {revolts}"
    );
    eprintln!(
        "TITHE ADJUDICATION: secondaries {slo}–{shi}, top-level conquests {plo}–{phi}, \
         branching ratio sigma {:.4}–{:.4} (The Tumult: 0.050–0.052); largest occupied bin {} \
         (sizes {}–{}), decades of support {:.2} (spec §5 wants >= 1.5); bin0/bin1 drop {:.1}x \
         (a heavy tail falls 2–4x)",
        slo as f64 / phi as f64,
        shi as f64 / plo as f64,
        largest,
        1u64 << largest,
        (1u64 << (largest + 1)) - 1,
        hornvale_kernel::math::log10(((1u64 << (largest + 1)) - 1) as f64),
        agg[0] as f64 / agg[1].max(1) as f64,
    );
    assert!(
        cascades >= MIN_POOLED_CASCADES,
        "the roll-downhill went inert: only {cascades} cascades pooled over the sample (floor \
         {MIN_POOLED_CASCADES}) across {raided} conquests — the branching ratio is structurally \
         zero and the shape question cannot be asked."
    );
    assert!(
        flights >= MIN_POOLED_FLIGHTS,
        "spec §4.3d's flight went inert: {flights} pooled over the sample (floor \
         {MIN_POOLED_FLIGHTS}) — a geometric verdict measured with a dead mechanism is a much \
         weaker finding than one measured with a live one."
    );
    assert!(
        revolts >= MIN_POOLED_REVOLTS,
        "spec §4.3d's revolt went inert: {revolts} pooled over the sample (floor \
         {MIN_POOLED_REVOLTS}) — the wounded-patron state (§4.3e) has stopped arising, and the \
         headline null would then be measuring the absence of the mechanism rather than its \
         failure to organize."
    );
}

/// **Spec §8.0 — strategies are actually various**, heavy. The criterion this
/// campaign added, and the one it can claim unambiguously: the pre-amendment
/// state had `assessment / eff_capacity` at exactly one value across all 2258
/// relations, so the model could not have distinguished a Sopranos bust-out
/// from a Roman census even in principle.
///
/// This gate binds the **observable projection** of that variety — how long a
/// standing relation has lasted, grouped by the patron people's authored
/// `time_horizon`, read straight off `History` with no probe. §4.3a's claim is
/// that a generational patron holds its vassal at maximum sustainable yield
/// and its relation persists, while an immediate one strips the stock; the
/// direct consequence is that relation lifetime rises with the horizon, which
/// is what is asserted.
///
/// **It is survivorship-biased and is stated as such**: only relations alive
/// at `now` are visible here, so this is "how long the survivors have stood",
/// not the lifetime of all relations ever formed. The unbiased distribution —
/// and the extraction-rate and fate columns §8.0 also asks for — are probe
/// measurements reported in the module docs, because `History` carries neither
/// a relation's remittances nor how a dissolved one ended.
///
/// The horizons are read from the psyche registry rather than written down, so
/// re-authoring a people cannot leave this test asserting a stale ordering.
///
/// # THE INSTRUMENT IS WHOLE-ROSTER NOW (The Underworld, 2026-08-18)
///
/// The Tense (2026-08-06) measured §4.3a's ordering **falsified** and left an
/// inverted two-point assertion behind as a tripwire: `rows[last]`'s median
/// against `rows[0]`'s, red if the ratio ever cleared 1.30 again. It fired on
/// this campaign's tree at **1.421** — and re-deriving it over the whole
/// roster, exactly as its own message instructed, showed the fire was the
/// instrument's and not the world's.
///
/// Both rosters, `SHAPE_SAMPLE` pooled. Nine peoples clear
/// [`MIN_RELATIONS_PER_PEOPLE`] on each tree — but **not the same nine**. The
/// sub-threshold rows are printed on this tree because the printout above now
/// reports every patron people; `main`'s run predates that and reported only
/// the nine it counted, so its sub-threshold rows read `under` rather than a
/// number:
///
/// ```text
///   people        horizon    n (main)  med (main)    n (ours)  med (ours)
///   gnoll            0.20         263      350 d          234      475 d
///   bugbear          0.30         442      275 d          335      250 d
///   goblin           0.50         102      525 d          115      350 d
///   hobgoblin        0.50         590      225 d          581      225 d
///   human            0.75         100      675 d          105      600 d
///   gully-dwarf      0.80       under          —           21     1325 d
///   kobold           0.80         527      175 d          484      175 d
///   drow             0.85         164      675 d          112      900 d
///   hill-dwarf       0.85         157      550 d          133      500 d
///   snow-elf         0.88       under          —           31      700 d
///   desert-dwarf     0.90         116      400 d           67      475 d
///   desert-elf       0.90       under          —           20      975 d
///   wood-elf         0.90       under          —           77      575 d
///   high-elf         0.95       under          —          196      675 d
/// ```
///
/// Note what the sub-threshold rows are doing at the top of the horizon range:
/// five of the six sit at horizon ≥ 0.80 with n between 20 and 77, and their
/// medians run from 475 d to 1325 d. Small-n medians of a survivorship-biased
/// age distribution are extremely noisy, and the highest horizons are exactly
/// where the roster is thinnest — which is the structural reason a top-end
/// order statistic is a poor instrument here whichever tree it is read on.
///
/// Three readings, and the third is the one that settles it:
///
/// 1. **The endpoint ratio changed which peoples it compares.** `main`'s top
///    row is desert-dwarf at 400 d (ratio 400/350 = **1.143**, green); ours is
///    high-elf at 675 d (675/475 = **1.421**, red). Desert-dwarf dropped under
///    [`MIN_RELATIONS_PER_PEOPLE`] and high-elf rose over it, so the verdict
///    turned on **roster composition at the inclusion threshold**. Neither
///    endpoint people is at an extreme of the *medians*: gnoll's 475 d is
///    fifth of nine on our tree, and kobold — second-highest horizon of all —
///    holds the shortest median on both trees.
/// 2. **The whole-roster rank correlation stays inside noise on every state
///    measured.** Spearman rho over (authored horizon, median
///    standing-relation age) reads **0.384** on `main` and **0.546** here,
///    against the roster's own two-tailed 5% bar of `NOISE_Z / sqrt(n - 1)` =
///    **0.693** at n = 9. A third state was measured as a control — decision
///    0145's node-index re-key neutralised on the merge product, everything
///    else of the campaign intact — and reads **0.629**, still under the bar
///    but *higher* than either shipped tree, with the endpoint ratio at
///    **2.20** (high-elf 825 d over gnoll 375 d). Three world states spread
///    rho across 0.245 and the endpoint ratio across 1.06 while none of them
///    crosses the noise bar: that spread IS the measurement's noise, and a
///    two-point ratio riding on it is not reading the relationship.
/// 3. **Peoples the registry authors at the SAME horizon spread as widely as
///    the endpoints do.** Horizon is constant within such a pair, so their
///    spread is this instrument's own noise floor measured on the roster
///    itself. On our tree drow/hill-dwarf (both 0.85) differ by **1.80×** and
///    goblin/hobgoblin (both 0.50) by **1.56×**; on `main`, goblin/hobgoblin
///    differ by **2.33×**. Every one of those brackets or exceeds the 1.421
///    the endpoint pair read across the full 0.20 → 0.95 span.
///
/// So the ordering has **not** returned. The Tense's null stands, and it is
/// recorded here as a *two-point artifact*, not a revived finding. What the
/// test asserts now is the whole-roster statistic — `|rho|` under the roster's
/// own noise bar — which the roster can falsify in either direction: a world
/// that genuinely orders lifetime by horizon drives rho toward +1 and fires
/// this, and one that genuinely inverts it drives rho toward −1 and fires it
/// too. The endpoint ratio is still computed and printed against
/// [`RETIRED_ENDPOINT_RATIO_BAR`], because the record of what it used to say is
/// worth keeping visible; nothing depends on it.
///
/// **The new assertion is not decorative, and that was measured rather than
/// argued.** Replacing each people's median with `horizon * 1000.0` — a
/// perfectly ordered roster and nothing else changed — drives rho to
/// **1.0000** and reddens this test, printing the POSITIVE branch of its
/// message. (`scripts/mutate.py`, reverted from a copy afterwards.) So the
/// tripwire fires on the state it exists to catch; it is not passing because
/// nothing could ever make it fail.
///
/// claim: readout(off-gate, heavy:) — standing-relation-age readout by
/// patron people over SHAPE_SAMPLE, asserting only the whole-roster rank
/// correlation against the roster's own noise bar
#[test]
#[ignore = "heavy: live-worldgen battery; deferred from the commit gate to the heavy set (decision 0132)"]
fn the_strategy_family_is_various() {
    let wc = WorldComponents::assemble().expect("registries");
    // Standing-relation ages, in standard days, grouped by the patron's people.
    let mut ages: BTreeMap<KindId, Vec<f64>> = BTreeMap::new();
    for s in SHAPE_SAMPLE {
        let h = history(s);
        let people: BTreeMap<_, _> = h
            .records
            .iter()
            .map(|r| (r.community, r.core.people))
            .collect();
        for t in &h.tribute {
            let p = *people.get(&t.patron).expect("a patron has a record");
            ages.entry(p).or_default().push(h.now - t.since);
        }
    }
    // Median age and authored horizon per patron people, in horizon order.
    // EVERY patron people is built and printed; the threshold filters what
    // enters the statistic, never what is reported. Which peoples sit either
    // side of that threshold is itself a moving quantity, and the retired
    // endpoint criterion turned on it — see this test's doc.
    let mut all_rows: Vec<(KindId, f64, usize, f64)> = ages
        .into_iter()
        .map(|(p, mut v)| {
            v.sort_by(f64::total_cmp);
            let horizon = wc
                .psyche
                .get(&p)
                .map(|m| m.time_horizon)
                .expect("a patron people carries a mind");
            (p, horizon, v.len(), v[v.len() / 2])
        })
        .collect();
    // Deterministic order: by horizon, ties broken by the people's stable id.
    all_rows.sort_by(|a, b| a.1.total_cmp(&b.1).then_with(|| a.0.0.cmp(b.0.0)));
    for (p, horizon, n, median) in &all_rows {
        eprintln!(
            "TITHE §8.0 patron {} (time_horizon {horizon}): {n} standing relations, \
             median age {median} d{}",
            p.0,
            if *n >= MIN_RELATIONS_PER_PEOPLE {
                ""
            } else {
                "   [under MIN_RELATIONS_PER_PEOPLE — reported, not counted]"
            }
        );
    }
    let rows: Vec<(KindId, f64, usize, f64)> = all_rows
        .iter()
        .copied()
        .filter(|(_, _, n, _)| *n >= MIN_RELATIONS_PER_PEOPLE)
        .collect();
    assert!(
        rows.len() >= 2,
        "fewer than two patron peoples hold {MIN_RELATIONS_PER_PEOPLE}+ standing relations \
         ({rows:?}) — §8.0's variety cannot be read at all, let alone falsified"
    );
    let (shortest, s_h, _, s_med) = rows[0];
    let (longest, l_h, _, l_med) = rows[rows.len() - 1];
    assert!(
        l_h > s_h,
        "the sampled patron peoples share one horizon ({s_h}), so this gate is vacuous"
    );
    // ---- FALSIFIED by The Tense (2026-08-06). Recorded, not rescued. ----
    //
    // §4.3a claims a generational patron's relation persists where an immediate
    // one's does not. Under era-varying capacity it does not hold: the roster
    // printed above is scattered, not ordered, and not converged onto a single
    // attractor either.
    //
    // The Tense left an INVERTED TWO-POINT assertion here as a tripwire —
    // rows[last]'s median against rows[0]'s, red if the ratio cleared 1.30 —
    // and warned in its own message that a return should be re-derived over
    // the whole roster before being trusted. The Underworld did that, and the
    // warning was right: the two-point reading flips verdict on which people
    // crosses MIN_RELATIONS_PER_PEOPLE at the top of the roster, while the
    // whole-roster rank correlation stays inside noise on both trees. The full
    // derivation, with both rosters side by side, is in this test's doc.
    //
    // What is asserted now is the whole-roster statistic. The endpoint ratio is
    // still computed and printed, so the retired reading stays visible, but it
    // decides nothing.
    let horizons: Vec<f64> = rows.iter().map(|r| r.1).collect();
    let medians: Vec<f64> = rows.iter().map(|r| r.3).collect();
    let rho = spearman(&horizons, &medians);
    let n = rows.len();
    let bar = NOISE_Z / ((n - 1) as f64).sqrt();

    // The same-horizon control. Two peoples the psyche registry authors at the
    // SAME horizon cannot differ BECAUSE of horizon, so the spread between
    // their medians is this instrument's own noise floor, measured on the
    // roster rather than argued for. Printed, not asserted: a re-authoring
    // that left no two peoples sharing a horizon would make it undefined, and
    // an assertion that silently stops applying is worse than none.
    let mut within_spread = 0.0f64;
    let mut within_where = String::from("none — no two peoples share a horizon");
    let mut i = 0;
    while i < rows.len() {
        let mut j = i;
        while j + 1 < rows.len() && rows[j + 1].1.total_cmp(&rows[i].1) == std::cmp::Ordering::Equal
        {
            j += 1;
        }
        if j > i {
            let group = &rows[i..=j];
            let lo = group.iter().map(|r| r.3).fold(f64::INFINITY, f64::min);
            let hi = group.iter().map(|r| r.3).fold(0.0f64, f64::max);
            if lo > 0.0 && hi / lo > within_spread {
                within_spread = hi / lo;
                within_where = format!(
                    "horizon {} ({})",
                    rows[i].1,
                    group
                        .iter()
                        .map(|r| format!("{} {} d", r.0.0, r.3))
                        .collect::<Vec<_>>()
                        .join(" vs ")
                );
            }
        }
        i = j + 1;
    }

    eprintln!(
        "TITHE §8.0 WHOLE-ROSTER: spearman(authored horizon, median standing-relation age) = \
         {rho:.4} over {n} patron peoples, against this roster's own two-tailed 5% noise bar \
         {NOISE_Z}/sqrt(n-1) = {bar:.4}. RETIRED two-point reading: {} (horizon {l_h}) {l_med} d \
         / {} (horizon {s_h}) {s_med} d = {:.3} against the retired \
         {RETIRED_ENDPOINT_RATIO_BAR} bar — printed, asserted on nothing. SAME-HORIZON control: \
         largest spread {within_spread:.3}x at {within_where}.",
        longest.0,
        shortest.0,
        l_med / s_med,
    );

    assert!(
        rho.abs() < bar,
        "the horizon/lifetime relationship is no longer indistinguishable from noise (spec \
         §8.0/§4.3a): spearman rho over (authored horizon, median standing-relation age) reads \
         {rho:.4} across {n} patron peoples, at or beyond this roster's two-tailed 5% bar of \
         {bar:.4}. A {} rho of that size is a finding either way — {}. The Tense measured this \
         claim FALSIFIED (2026-08-06) and The Underworld re-derived the null over the whole \
         roster (2026-08-18, rho 0.384 on main and 0.546 on its own tree); re-derive it again \
         over the roster printed above, and do NOT restore the retired two-point ratio, which \
         flips verdict on roster composition alone. See this test's doc comment.",
        if rho > 0.0 { "POSITIVE" } else { "NEGATIVE" },
        if rho > 0.0 {
            "§4.3a's ordering may genuinely have returned"
        } else {
            "lifetime may genuinely fall with horizon, which §4.3a does not predict either"
        },
    );
}

/// Spearman rank correlation between two equal-length series, ties taking
/// average ranks. Deterministic: `total_cmp` throughout, no float equality.
///
/// A local copy of the helper `windows/lab/tests/disposition_calibration.rs`
/// carries, because a test in `windows/worldgen` cannot depend on one in
/// `windows/lab` and neither belongs in a shipped crate.
fn spearman(xs: &[f64], ys: &[f64]) -> f64 {
    fn ranks(v: &[f64]) -> Vec<f64> {
        let mut idx: Vec<usize> = (0..v.len()).collect();
        idx.sort_by(|a, b| v[*a].total_cmp(&v[*b]));
        let mut out = vec![0.0; v.len()];
        let mut i = 0;
        while i < idx.len() {
            let mut j = i;
            while j + 1 < idx.len()
                && v[idx[j + 1]].total_cmp(&v[idx[i]]) == std::cmp::Ordering::Equal
            {
                j += 1;
            }
            let avg = (i + j) as f64 / 2.0;
            for k in idx.iter().take(j + 1).skip(i) {
                out[*k] = avg;
            }
            i = j + 1;
        }
        out
    }
    assert_eq!(xs.len(), ys.len(), "spearman needs paired series");
    let (rx, ry) = (ranks(xs), ranks(ys));
    let n = rx.len() as f64;
    let (mx, my) = (rx.iter().sum::<f64>() / n, ry.iter().sum::<f64>() / n);
    let num: f64 = rx.iter().zip(&ry).map(|(a, b)| (a - mx) * (b - my)).sum();
    let den = (rx.iter().map(|a| (a - mx).powi(2)).sum::<f64>()
        * ry.iter().map(|b| (b - my).powi(2)).sum::<f64>())
    .sqrt();
    if den == 0.0 { 0.0 } else { num / den }
}

/// The rank correlation above, against orderings whose answer is known by
/// hand — including the tied case, which is the one the roster actually hits
/// (two peoples at horizon 0.50, two more at 0.85).
#[test]
fn spearman_reads_known_orderings() {
    assert!(
        (spearman(&[1.0, 2.0, 3.0], &[10.0, 20.0, 30.0]) - 1.0).abs() < 1e-12,
        "perfect ascent is +1"
    );
    assert!(
        (spearman(&[1.0, 2.0, 3.0], &[30.0, 20.0, 10.0]) + 1.0).abs() < 1e-12,
        "perfect descent is -1"
    );
    assert_eq!(
        spearman(&[1.0, 2.0, 3.0], &[5.0, 5.0, 5.0]),
        0.0,
        "no variance in y is 0, not NaN"
    );
    // The roster's own shape at 2026-08-18: nine peoples, two tied pairs of
    // horizons, medians as measured on this campaign's tree. Recomputed by
    // hand as 32.5 / sqrt(59 * 60).
    let horizons = [0.20, 0.30, 0.50, 0.50, 0.75, 0.80, 0.85, 0.85, 0.95];
    let medians = [
        475.0, 250.0, 350.0, 225.0, 600.0, 175.0, 900.0, 500.0, 675.0,
    ];
    assert!(
        (spearman(&horizons, &medians) - 0.546_23).abs() < 1e-4,
        "the measured roster reads 0.5462, under the n=9 noise bar of 0.6930"
    );
}
