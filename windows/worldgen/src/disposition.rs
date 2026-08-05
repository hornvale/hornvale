//! The per-settlement disposition draw (The Tolerance): a species is a
//! distribution, and this is where a *specific* settlement's mind is drawn
//! out of it.
//!
//! A composition-root concern by construction. The draw needs
//! [`hornvale_species::Dispersion`] (a species datum) and the occupation's own
//! site and founding year (a settlement/history datum), and a domain crate may
//! not depend on a sibling — so `windows/worldgen` is the only legal home, and
//! [`crate::streams::SETTLEMENT_DISPOSITION`] is a composition-root label for
//! the same reason `religion/deity/v2` is.
//!
//! ## Two entry points, one arithmetic
//!
//! [`people_disposition`] is the derivation: a pure function of the draw key.
//! [`settlement_disposition`] is a thin wrapper that reads that key off a
//! committed ledger entity and calls it.
//!
//! The split is not stylistic. The raid gate this feeds (`Bake::
//! takes_the_initiative`) fires *during* the deep-history bake, on `Bake`
//! communities that are not yet ledger entities — it cannot read a ledger, so
//! it calls [`people_disposition`] directly. If the two sides ever consumed
//! different streams or ran different arithmetic, a world would *report* a
//! disposition its own history was not *baked with*: silent, deterministic,
//! and catastrophic. `windows/worldgen/tests/tolerance_draw.rs` pins their
//! agreement.

use hornvale_kernel::seed::StreamLabel;
use hornvale_kernel::{CellId, ComponentStore, EntityId, KindId, Seed, Stream, Value, World};
use hornvale_species::{Dispersion, MindVector};

/// Half-width of a symmetric uniform offset with unit standard deviation:
/// √3, as the nearest `f64`. A uniform on `[−√3·σ, +√3·σ]` has mean exactly 0
/// and standard deviation exactly σ, which is what
/// [`hornvale_species::Dispersion`]'s authored frame says it is (spec D2: the
/// authored vector is the MEAN, the dispersion is the STANDARD DEVIATION).
///
/// A literal rather than `3f64.sqrt()` so it is a visible save-format
/// constant; `the_unit_halfwidth_is_the_square_root_of_three` below pins that
/// it *is* `sqrt(3.0)` (IEEE-exact, so portable).
/// type-audit: bare-ok(ratio)
const UNIT_SD_HALFWIDTH: f64 = 1.732_050_807_568_877_2;

/// The dispersion of a people that has no authored row: a point, not a
/// distribution — exactly the model's behaviour before this campaign. Task
/// 2's `every_kind_with_a_mind_carries_a_dispersion` keeps this unreachable
/// for the shipped roster; it exists so a caller holding a partial store
/// degrades to the authored location rather than to `None`.
const NO_SPREAD: Dispersion = Dispersion {
    mind: 0.0,
    society: 0.0,
    perception: 0.0,
};

/// Reduce an occupation's founding year to the stable integer the draw key
/// uses.
///
/// **Both sides of the draw must call this.** The bake holds `founded` as a
/// raw `f64`; the ledger holds the same year after `Ledger::commit` quantized
/// it to 8 significant digits (`hornvale_kernel::quantize`). Keying on the
/// float itself would therefore derive two *different* streams for one
/// settlement, with nothing red to show for it. The default bake grid
/// (`BakeConfig::default_millennia`) steps 0 → 2000 by 25, so every year is
/// an exact integer and rounding is lossless in both directions;
/// `the_year_key_survives_the_ledgers_quantization` pins that across the whole
/// grid.
/// type-audit: bare-ok(count: founded), bare-ok(count: return)
pub fn occupation_draw_key(founded: f64) -> i64 {
    founded.round() as i64
}

/// The one place the draw key becomes a stream. **Every** caller goes through
/// here, so the leg-string format (`"{site}/{founded_year}"`) is written once
/// and cannot drift between the two entry points; `the_draw_is_byte_pinned_
/// for_a_known_key` pins the format itself.
fn draw_stream(seed: Seed, site: CellId, founded_year: i64) -> Stream {
    // The dynamic leg IS the key: site and founding year, joined. Composed
    // under the flat `settlement/disposition/v1` root exactly as the deity
    // stream composes its per-settlement leg.
    let leg = format!("{}/{}", site.0, founded_year);
    seed.derive(crate::streams::SETTLEMENT_DISPOSITION)
        .derive(StreamLabel::dynamic(&leg))
        .stream()
}

/// One axis's perturbation: consume one draw and offset `location` by it,
/// scaled by `spread` and clamped to the axis's closed `[0, 1]` range.
///
/// **Consumes exactly one draw**, which is what lets a caller that needs only
/// the *first* axis ([`drawn_threat_response`]) reach the identical value the
/// three-axis [`people_disposition`] would put there.
fn perturb(stream: &mut Stream, location: f64, spread: f64) -> f64 {
    // `next_f64` is [0, 1), so this is [−1, 1) — asymmetric by one ULP at
    // the top, which is standard for a uniform built this way and is not
    // worth a rejection loop.
    let unit = stream.next_f64() * 2.0 - 1.0;
    (location + unit * UNIT_SD_HALFWIDTH * spread).clamp(0.0, 1.0)
}

/// A settlement's drawn `threat_response` alone — the **bake-side** read, and
/// the input the raid gate (`Bake::takes_the_initiative`) now reads instead of
/// its people's authored species constant.
///
/// Kernel types only, by design. `windows/worldgen/src/history_bake.rs` states
/// that "the bake reads only kernel types", so the authored location and the
/// authored spread arrive here as two bare `f64`s that the composition root
/// resolved off `hornvale_species` — the same channel `BakeConfig.disposition`
/// and `BakeConfig.in_group_radius` already travel. No `hornvale-species` type
/// crosses into the bake.
///
/// **This is the first of [`people_disposition`]'s three draws, not a fourth
/// one.** `threat_response` is the first axis perturbed there, both functions
/// build their stream through [`draw_stream`], and both offset through
/// [`perturb`], so for one key this returns exactly
/// `people_disposition(..).unwrap().threat_response` — asserted by
/// `windows/worldgen/tests/tolerance_draw.rs`'s
/// `the_gate_side_scalar_is_the_full_draws_threat_response`. Consuming one draw
/// rather than three is not a shortcut around that agreement: the axes are
/// independent draws in a frozen order, so the first is the first either way.
///
/// A `spread` of zero returns `location` unchanged — exactly the model's
/// behaviour before this campaign, which is what makes Task 5's mutation proof
/// (zero dispersion ⇒ zero between-settlement variance) a statement about this
/// function rather than about the gate that calls it.
/// type-audit: bare-ok(count: founded_year), bare-ok(ratio: location), bare-ok(ratio: spread), bare-ok(ratio: return)
pub fn drawn_threat_response(
    seed: Seed,
    site: CellId,
    founded_year: i64,
    location: f64,
    spread: f64,
) -> f64 {
    perturb(&mut draw_stream(seed, site, founded_year), location, spread)
}

/// A settlement's effective mind: its people's authored [`MindVector`],
/// perturbed per dimension by a draw scaled by that people's
/// [`Dispersion::mind`].
///
/// # The key
///
/// `(site, founded_year)` — "this settlement, founded *here*, *then*", read
/// off the occupation record. Deliberately **not**:
///
/// - the settlement's `EntityId` (spec D3 / The Salt: `Ledger::mint_entity` is
///   a sequential counter, so minting one extra entity earlier would reshuffle
///   every settlement's psychology);
/// - its `BakeId`/`Community.lineage` (the same counter in another costume,
///   and *circular* inside the bake: disposition drives raiding drives
///   founding drives `BakeId` assignment);
/// - its bare `cell-id` alone, which is **not unique over occupations**. The
///   bake's `node_index` holds one *alive* community per cell, so bare
///   `cell-id` separates the settlements standing at `now` — but nothing more
///   than that. `Bake::vacant_habitable` only excludes cells an *alive*
///   community holds, so a dead community's cell is re-settleable, and
///   `Bake::relocate`'s conquest path opens the raider's record at the
///   victim's cell in the very year the victim's record closes. Successive
///   occupations of one site are different settlements with different
///   histories; a bare cell key would hand them one and the same mind.
///
/// A relocation never edits a record — it opens a *new* one (`Bake::open`,
/// reached from both of `relocate`'s branches), which this model already
/// treats as a new occupation everywhere else. So a record's own `site` and
/// `founded` are immutable once opened, and the pair separates occupations
/// that share a site, which is exactly what the cell alone cannot do.
///
/// # The key's uniqueness, as MEASURED rather than assumed
///
/// `(site, founded_year)` is **unique among the settlements alive at `now`**,
/// which is the entire domain of [`settlement_disposition`] — a ruin commits no
/// `cell-id`, so the wrapper is undefined on one. The ledger-side contract is
/// sound.
///
/// It is **not** unique over *all* occupation records: 92 of 862 at seed 1, 130
/// of 919 at seed 42, replicated over seeds 1..=12 at roughly 3–15%. Every one
/// of those collisions has the same shape, asserted in
/// `the_draw_key_is_reachable_and_its_uniqueness_has_the_measured_shape`: at
/// most one alive record per colliding group, and always at least one
/// **zero-tenure** record — a community opened and closed inside one epoch,
/// which is what `relocate`'s conquest path produces when it opens the raider's
/// record at the victim's cell in the same year the victim's closes.
///
/// So a within-epoch transient can share a drawn mind with the record that
/// displaced it. Two *simultaneously alive* communities never can
/// (`Bake.node_index` holds one alive community per cell). It was not resolved
/// here by adding a third key component, because every available candidate
/// (`BakeId`, a within-year sequence number, population) is either the
/// sequential-counter trap or circular through raiding itself.
///
/// **Task 4's ruling: it is accepted, and it does not matter to the raid gate.**
/// Three reasons, in the order they bind:
///
/// 1. **It is a correlation, not a wrong value.** A colliding pair draws the
///    same *unit offset*, but each record applies it to its own people's
///    authored location and spread, so two records of different peoples get
///    different dispositions anyway, and two of the same people get one that is
///    a perfectly legitimate draw from that people's distribution. Nothing
///    lands outside the authored support; only the joint distribution of one
///    rare pair is degenerate.
/// 2. **The gate's blast radius for a transient is one decision.** A
///    zero-tenure record is opened and closed inside one epoch. The only use it
///    ever makes of its disposition is deciding whether the remnant it becomes
///    may look at held ground on its way down the cascade — one branch of
///    `Bake::best_home`, taken once, on a band that is about to stop existing.
///    It never grows, never takes a vassal, and never commits a settlement
///    fact, so the correlation cannot reach the ledger.
/// 3. **It cannot reach the campaign's instrument.** H1 and H2 are measured
///    over settlements alive at `now`, where the key IS unique; a
///    zero-tenure transient is not in that population, and Task 5's mutation
///    proof (zero dispersion ⇒ zero between-settlement variance) holds
///    identically whether or not two dead records shared an offset — at spread
///    0 every draw returns its people's authored location regardless of key.
///
/// The alternative — inventing a third key component to decorrelate a pair that
/// makes one throwaway decision each — would buy nothing measurable and would
/// have to be paid for in exactly the currency spec D3 rules out.
///
/// (An earlier draft of this reasoning leaned on a ratified ruling, numbered
/// just past the end of the log, said to retire one-community-per-cell. No such
/// record exists — `docs/decisions/` ends at 0097 — and the invariant is live in
/// `Bake.node_index`, not scheduled for removal. The argument above is the one
/// the code actually supports, and it is checked rather than cited.)
///
/// # The draw
///
/// Three **independent** draws, one per dimension, in `MindVector`'s
/// declaration order (`threat_response`, `deliberation_latency`,
/// `time_horizon`) — a frozen consumption order, like every other stream in
/// this workspace. All three share the single per-vector `Dispersion::mind`
/// scale, because dispersion is authored per *vector*, not per dimension (a
/// per-dimension spread is an explicit spec non-goal). Sharing one *draw*
/// across the three would instead make them perfectly correlated, which "a
/// spread around a point in 3-space" does not imply.
///
/// Each offset is uniform on `[−√3·σ, +√3·σ]`: mean 0, standard deviation
/// exactly σ, bounded support, and no transcendental in the path.
///
/// # The clamp, and the bias it induces — a disclosed consequence, not a bug
///
/// Results are clamped to `[0, 1]`, the closed range every `MindVector` scalar
/// is defined on. **On a bounded axis, spec D2's "the authored value is the
/// mean" and "the spread is symmetric" are incompatible near a boundary**, and
/// the clamp resolves that conflict in favour of the bound. The cost is real
/// and is stated here so it is not rediscovered later as a mystery: a people
/// authored near a boundary piles clamped mass on it, pulling its *realized*
/// mean **toward the interior** — away from whichever bound it presses. Near
/// the upper bound that means *below* the authored value; near the lower bound
/// it means *above* it. Both occur on the shipped roster.
///
/// Which axes this actually touches, computed from the authored σ rather than
/// eyeballed (half-width √3·σ, matching the uniform draw above; rows with any
/// clamped mass, ordered by the size of the shift):
///
/// ```text
///   people      σ(mind)  axis                  authored  clamped mass   realized    shift
///   ----------  -------  --------------------  --------  ------------   --------  --------
///   human          0.35  time_horizon              0.75  29.4% upper      0.6977   -0.0523
///   gnoll          0.22  threat_response           0.85  30.3% upper      0.8150   -0.0350
///   gnoll          0.22  deliberation_latency      0.20  23.8% LOWER      0.2215   +0.0215
///   gnoll          0.22  time_horizon              0.20  23.8% LOWER      0.2215   +0.0215
///   human          0.35  deliberation_latency      0.60  17.0% up/0.5% dn 0.5825   -0.0175
///   bugbear        0.20  threat_response           0.80  21.1% upper      0.7845   -0.0155
///   *-dragon       0.08  threat_response           0.95  32.0% upper      0.9358   -0.0142
///   *-dragon       0.08  time_horizon              0.90  13.9% upper      0.8973   -0.0027
///   bugbear        0.20  time_horizon              0.30   6.7% LOWER      0.3016   +0.0016
///   kobold         0.12  threat_response           0.80   1.9% upper      0.7999   -0.0001
///   kobold         0.12  time_horizon              0.80   1.9% upper      0.7999   -0.0001
///   human          0.35  threat_response           0.50  8.8% up/8.8% dn  0.5000   -0.0000
/// ```
///
/// Four things that table says and prose about "peoples authored near a
/// boundary" does not:
///
/// - **`human` is the most-clamped people on the roster**, not an unaffected
///   centred one. Its σ = 0.35 is the *widest* authored, which is precisely
///   what makes it clamp hardest: its half-width is 0.606, so every one of its
///   three axes reaches a bound. `time_horizon` carries the largest realized
///   shift on the roster, −0.052 (≈7% of the authored value).
/// - **`hobgoblin` and `goblin` clamp nothing at all.** hobgoblin's σ = 0.10
///   gives a half-width of 0.173, so its highest axis (0.7) tops out at 0.873;
///   goblin sits at 0.5 on all three with a half-width of 0.433. Neither can
///   reach a bound, and neither belongs on a list of affected peoples.
/// - **The lower bound clamps too.** gnoll's `deliberation_latency` and
///   `time_horizon` (0.20) and bugbear's `time_horizon` (0.30) press *downward*
///   and are pulled *up*. A one-sided "the mean comes out low" reading would
///   mis-sign these.
/// - **Being centred is not what protects a people; symmetry is.** human's
///   `threat_response` sits at 0.5 and clamps 8.8% at *each* bound, so the two
///   biases cancel exactly and the shift is 0.0000. The bias is a function of
///   asymmetry with respect to the bounds, not of distance from 0.5.
///
/// The three chromatic dragons are listed for completeness because
/// [`people_disposition`] is defined on any minded kind. They are `Solitary`
/// and never settle, so [`settlement_disposition`] never resolves one and H1's
/// per-settlement measurement never sees them.
///
/// The campaign's preregistered **H1 ("the mean survives")** must be read
/// against this table: a realized mean displaced toward the interior on one of
/// these axes is the clamp, not a biased draw.
///
/// No distribution scheme (reflection, logit-space, σ-rescaling near the
/// boundary) was chosen to make the mean come out right — that would be
/// retuning to rescue a frozen prediction, which this campaign forbids.
///
/// **This is an H1 disclosure only; H3 is untouched, and not by luck.** The
/// raid gate is a *threshold* at `RAID_DISPOSITION_MIN = 0.6`. Clamping to
/// `[0, 1]` is a monotone map that fixes every value already inside the range —
/// including 0.6 itself — so `x > 0.6` if and only if `clamp(x) > 0.6`, for
/// both bounds and for any threshold strictly inside `(0, 1)`. Mass absorbed at
/// 1.0 was already above the threshold; mass absorbed at 0.0 was already below
/// it. No draw changes which side of the gate it falls on, so the clamp moves
/// H1's reported means and **cannot** move H3's rates.
///
/// Returns `None` when `people` has no authored mind at all. A people with a
/// mind but no authored dispersion draws its location exactly (see
/// [`NO_SPREAD`]).
/// type-audit: bare-ok(count: founded_year), bare-ok(identifier-text: people)
pub fn people_disposition(
    seed: Seed,
    site: CellId,
    founded_year: i64,
    people: &str,
    psyche: &ComponentStore<KindId, MindVector>,
    dispersion: &ComponentStore<KindId, Dispersion>,
) -> Option<MindVector> {
    let location = *psyche.get_by_label(people)?;
    let spread = dispersion
        .get_by_label(people)
        .copied()
        .unwrap_or(NO_SPREAD);
    let mut stream = draw_stream(seed, site, founded_year);
    let mut next = |location: f64| perturb(&mut stream, location, spread.mind);
    Some(MindVector {
        threat_response: next(location.threat_response),
        deliberation_latency: next(location.deliberation_latency),
        time_horizon: next(location.time_horizon),
    })
}

/// The ledger-side read of [`people_disposition`]: a settlement's effective
/// mind, from its committed facts alone.
///
/// Reads the draw key off `settlement` — `hornvale_settlement::CELL_ID` (its
/// site) and `hornvale_history::OCC_FOUNDED` (its founding year, reduced
/// through [`occupation_draw_key`]) — plus `hornvale_history::OCC_PEOPLE`, and
/// resolves the authored registries at the composition root. `None` when the
/// entity is not a history-emitted settlement, or when its people has no
/// authored mind.
///
/// **This is not the function the bake calls.** See the module docs: the raid
/// gate runs before these entities exist and calls [`people_disposition`]
/// directly with the same key.
pub fn settlement_disposition(world: &World, settlement: EntityId) -> Option<MindVector> {
    let site = match world
        .ledger
        .value_of(settlement, hornvale_settlement::CELL_ID)?
    {
        Value::Number(n) => CellId(*n as u32),
        _ => return None,
    };
    let founded = match world
        .ledger
        .value_of(settlement, hornvale_history::OCC_FOUNDED)?
    {
        Value::Number(n) => *n,
        _ => return None,
    };
    let people = world
        .ledger
        .text_of(settlement, hornvale_history::OCC_PEOPLE)?;
    people_disposition(
        world.seed,
        site,
        occupation_draw_key(founded),
        people,
        &hornvale_species::psyche_registry(),
        &hornvale_species::dispersion_registry(),
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn the_unit_halfwidth_is_the_square_root_of_three() {
        assert_eq!(UNIT_SD_HALFWIDTH, 3.0f64.sqrt());
    }

    #[test]
    fn the_year_key_rounds_to_the_nearest_whole_year() {
        assert_eq!(occupation_draw_key(0.0), 0);
        assert_eq!(occupation_draw_key(725.0), 725);
        assert_eq!(occupation_draw_key(2000.0), 2000);
        assert_eq!(occupation_draw_key(-25.0), -25);
    }

    /// The realized standard deviation of the draw is the authored one, on an
    /// axis far enough from both bounds that no clamping occurs. This is the
    /// arithmetic behind spec D2; the clamped case is the disclosed bias.
    #[test]
    fn an_unclamped_axis_realizes_the_authored_standard_deviation() {
        let psyche: ComponentStore<KindId, MindVector> = [(
            KindId("probe"),
            MindVector {
                threat_response: 0.5,
                deliberation_latency: 0.5,
                time_horizon: 0.5,
            },
        )]
        .into_iter()
        .collect();
        let dispersion: ComponentStore<KindId, Dispersion> = [(
            KindId("probe"),
            Dispersion {
                mind: 0.1,
                society: 0.0,
                perception: 0.0,
            },
        )]
        .into_iter()
        .collect();
        let n = 20_000u32;
        let mut sum = 0.0;
        let mut sum_sq = 0.0;
        for cell in 0..n {
            let v = people_disposition(Seed(42), CellId(cell), 0, "probe", &psyche, &dispersion)
                .expect("the probe kind carries a mind")
                .threat_response;
            assert!(
                (0.0..=1.0).contains(&v),
                "0.5 ± √3·0.1 cannot leave [0, 1]; this axis must be unclamped"
            );
            sum += v;
            sum_sq += v * v;
        }
        let mean = sum / f64::from(n);
        let sd = (sum_sq / f64::from(n) - mean * mean).sqrt();
        assert!(
            (mean - 0.5).abs() < 0.005,
            "unclamped realized mean {mean} drifted off the authored 0.5"
        );
        assert!(
            (sd - 0.1).abs() < 0.005,
            "realized sd {sd} is not the authored dispersion 0.1"
        );
    }
}
