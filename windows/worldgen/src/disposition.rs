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
use hornvale_kernel::{CellId, ComponentStore, EntityId, KindId, Seed, Value, World};
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
/// (`Bake.node_index` holds one alive community per cell). **Task 4 owns
/// whether that matters to the raid gate**; it was not resolved here by adding
/// a third key component, because every available candidate (`BakeId`, a
/// within-year sequence number, population) is either the sequential-counter
/// trap or circular through raiding itself.
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
/// mean back off the boundary, below its *authored* one.
///
/// Which peoples this touches, on the shipped roster: gnoll (`threat_response`
/// 0.85, `mind` dispersion 0.22), kobold and bugbear (0.8), and hobgoblin
/// (0.7) all press the upper bound. Human and goblin sit at the centred 0.5
/// and are unaffected. The campaign's preregistered **H1 ("the mean
/// survives")** must be read against this: a small downward shift in those
/// peoples' realized means is the clamp, not a biased draw.
///
/// No distribution scheme (reflection, logit-space, σ-rescaling near the
/// boundary) was chosen to make the mean come out right — that would be
/// retuning to rescue a frozen prediction, which this campaign forbids.
///
/// One thing the bias does **not** reach: the raid gate is a *threshold* at
/// `RAID_DISPOSITION_MIN = 0.6`, and clamping at 1.0 moves no mass across 0.6.
/// So the clamp shows up in H1's reported means and *not* in H3's rates.
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
    // The dynamic leg IS the key: site and founding year, joined. Composed
    // under the flat `settlement/disposition/v1` root exactly as the deity
    // stream composes its per-settlement leg.
    let leg = format!("{}/{}", site.0, founded_year);
    let mut stream = seed
        .derive(crate::streams::SETTLEMENT_DISPOSITION)
        .derive(StreamLabel::dynamic(&leg))
        .stream();
    let mut perturb = |location: f64| {
        // `next_f64` is [0, 1), so this is [−1, 1) — asymmetric by one ULP at
        // the top, which is standard for a uniform built this way and is not
        // worth a rejection loop.
        let unit = stream.next_f64() * 2.0 - 1.0;
        (location + unit * UNIT_SD_HALFWIDTH * spread.mind).clamp(0.0, 1.0)
    };
    Some(MindVector {
        threat_response: perturb(location.threat_response),
        deliberation_latency: perturb(location.deliberation_latency),
        time_horizon: perturb(location.time_horizon),
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
