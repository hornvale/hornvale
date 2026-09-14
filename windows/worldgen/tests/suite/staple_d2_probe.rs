//! The Staple D2 paired probe contract and integrated treatment report.
//!
//! Task 1 froze the report shape before exchange existed. Task 4 drives that
//! contract from the integrated bake while keeping the seed roster, verdict
//! bars, and anti-vacuity rules local to the probe.

use hornvale_astronomy::SkyPins;
use hornvale_kernel::{Seed, World, test_lineage};
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{
    BuildDepth, ExchangeCensus, ExchangeTreatment, SettlementPins, WorldComponents, build_world_to,
    build_world_with_exchange_treatment, collapse_events, occupation_records,
};

const PROBE_WORLD_DENOMINATOR: usize = 200;
const PROBE_SEEDS: std::ops::RangeInclusive<u64> = 1..=200;

/// The Staple D2's FROZEN preregistered settlement-count bar. It read as "the
/// live band in `history_tumult.rs`" until The Trencher retired that band's
/// ceiling (adopting The Tidemark's `b5edf6106`): `history_tumult` and
/// `history_placement` now assert a minimum viable floor and NO upper bound,
/// so this constant no longer mirrors anything live. It is left at `40..=400`
/// deliberately — decision 0016 forbids retuning another campaign's
/// preregistered bar — but it is a frozen bar, not a reflection.
///
/// **AND ITS UPPER HALF IS NOW PROBABLY DEAD, WHICH IS A FINDING, NOT A
/// REPAIR ORDER.** This probe counts TREATMENT-ONLY breaches: a world where
/// control and treatment both breach is deliberately not counted, which
/// `demographic_instability_counts_only_treatment_only_bar_breaches` pins
/// directly (its `pairs[1]` sets control AND treatment to 401 and expects a
/// count of 1, from the unrelated `39` case). So once the world's baseline
/// settlement count clears 400 everywhere — seed 42 reads 413 on this branch
/// — both arms breach on every world and this bar's settlement half can
/// never fire again. Both live probes are `#[ignore]`d, so nothing has
/// observed that yet; the next run would read a silent zero as health. Whoever
/// re-runs The Staple D2 owns the decision, and should re-derive the counts
/// rather than trusting this note.
const SETTLEMENT_COUNT_BAR: std::ops::RangeInclusive<usize> = 40..=400;

/// The live depopulation ceiling. The D2 design names `history_tumult.rs` as
/// its source, but identifier verification found this constant in
/// `history_sundering.rs`; Task 1 records that citation correction in the
/// campaign ledger rather than inventing a replacement bar.
const MAX_COLLAPSE_SHARE: f64 = 0.05;

/// The live not-depopulated floor in `history_tumult.rs`.
const MIN_ALIVE_AT_NOW: u64 = 50;

/// More than half of the 200 treatment worlds is the instability dead pole.
const INSTABILITY_BREACH_FLOOR: usize = PROBE_WORLD_DENOMINATOR / 2;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum AttemptStatus {
    Proposed,
    Accepted,
    Settled,
    Partial,
    Refused,
    Impossible,
}

#[derive(Clone, Copy, Debug, PartialEq)]
struct DemographicReading {
    settlement_count: usize,
    collapse_share: f64,
    alive_at_now: u64,
}

impl DemographicReading {
    fn from_world(world: &World) -> Self {
        let occupations = occupation_records(world);
        assert!(
            !occupations.is_empty(),
            "a demographic reading needs at least one occupation"
        );
        Self {
            settlement_count: hornvale_settlement::all_settlements(world).len(),
            collapse_share: collapse_events(world) as f64 / occupations.len() as f64,
            alive_at_now: occupations
                .iter()
                .filter(|record| record.core.ended.is_none())
                .count() as u64,
        }
    }

    fn breaches_settlement_count(self) -> bool {
        !SETTLEMENT_COUNT_BAR.contains(&self.settlement_count)
    }

    fn breaches_collapse_share(self) -> bool {
        self.collapse_share > MAX_COLLAPSE_SHARE
    }

    fn breaches_alive_at_now(self) -> bool {
        self.alive_at_now < MIN_ALIVE_AT_NOW
    }
}

#[derive(Clone, Debug, PartialEq)]
struct PairObservation {
    seed: u64,
    control: DemographicReading,
    treatment: DemographicReading,
    attempts: AttemptCounts,
    stock_conservation_residuals: [f64; 2],
    disabled_control_is_byte_identical: bool,
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
struct AttemptCounts {
    attempted: usize,
    proposed: usize,
    accepted: usize,
    settled: usize,
    partial: usize,
    refused: usize,
    impossible: usize,
}

impl AttemptCounts {
    fn from_statuses(statuses: &[AttemptStatus]) -> Self {
        let count = |wanted| statuses.iter().filter(|&&status| status == wanted).count();
        Self {
            attempted: statuses.len(),
            proposed: count(AttemptStatus::Proposed),
            accepted: count(AttemptStatus::Accepted),
            settled: count(AttemptStatus::Settled),
            partial: count(AttemptStatus::Partial),
            refused: count(AttemptStatus::Refused),
            impossible: count(AttemptStatus::Impossible),
        }
    }

    fn from_census(census: ExchangeCensus) -> Self {
        Self {
            attempted: census.attempts as usize,
            proposed: census.proposed as usize,
            accepted: census.accepted as usize,
            settled: census.settled as usize,
            partial: census.partial as usize,
            refused: census.refused as usize,
            impossible: census.impossible as usize,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct CountWithAttempts {
    count: usize,
    attempts: std::num::NonZeroUsize,
}

impl CountWithAttempts {
    fn new(count: usize, attempts: std::num::NonZeroUsize) -> Self {
        assert!(
            count <= attempts.get(),
            "an outcome count cannot exceed its attempt denominator"
        );
        Self { count, attempts }
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
struct TreatmentOnlyBreaches {
    settlement_count: usize,
    collapse_share: usize,
    alive_at_now: usize,
}

impl TreatmentOnlyBreaches {
    fn crosses_instability_pole(self) -> bool {
        self.settlement_count > INSTABILITY_BREACH_FLOOR
            || self.collapse_share > INSTABILITY_BREACH_FLOOR
            || self.alive_at_now > INSTABILITY_BREACH_FLOOR
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
struct StockConservationResiduals {
    seed: u64,
    by_resource: [f64; 2],
}

#[derive(Clone, Debug, PartialEq)]
struct ProbeReport {
    world_denominator: usize,
    activation_worlds: usize,
    attempted: usize,
    proposed: CountWithAttempts,
    accepted: CountWithAttempts,
    settled: CountWithAttempts,
    partial: CountWithAttempts,
    refused: CountWithAttempts,
    impossible: CountWithAttempts,
    stock_conservation_residuals: Vec<StockConservationResiduals>,
    treatment_only_breaches: TreatmentOnlyBreaches,
    byte_identical_disabled_controls: usize,
}

impl ProbeReport {
    fn is_zero_activation(&self) -> bool {
        self.activation_worlds == 0
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ProbeContractError {
    WrongWorldDenominator {
        actual: usize,
    },
    UnexpectedSeed {
        index: usize,
        expected: u64,
        actual: u64,
    },
    NonFiniteStockResidual {
        seed: u64,
        resource_index: usize,
    },
    NonZeroStockResidual {
        seed: u64,
        resource_index: usize,
    },
    DisabledControlChanged {
        seed: u64,
    },
    NoExchangeAttempts,
}

/// Reduce the fixed paired roster to the exact Task 0 report.
///
/// Validation is deliberately ordered before the zero-attempt result: an
/// inert pre-production treatment must still prove the 200-world roster,
/// finite, exactly zero conservation readings, and disabled-control identity.
/// Only then is `NoExchangeAttempts` a meaningful result rather than a
/// vacuous report.
fn summarize_probe(pairs: &[PairObservation]) -> Result<ProbeReport, ProbeContractError> {
    if pairs.len() != PROBE_WORLD_DENOMINATOR {
        return Err(ProbeContractError::WrongWorldDenominator {
            actual: pairs.len(),
        });
    }

    for (index, (pair, expected)) in pairs.iter().zip(PROBE_SEEDS).enumerate() {
        if pair.seed != expected {
            return Err(ProbeContractError::UnexpectedSeed {
                index,
                expected,
                actual: pair.seed,
            });
        }
        for (resource_index, residual) in pair.stock_conservation_residuals.iter().enumerate() {
            if !residual.is_finite() {
                return Err(ProbeContractError::NonFiniteStockResidual {
                    seed: pair.seed,
                    resource_index,
                });
            }
            if *residual != 0.0 {
                return Err(ProbeContractError::NonZeroStockResidual {
                    seed: pair.seed,
                    resource_index,
                });
            }
        }
        if !pair.disabled_control_is_byte_identical {
            return Err(ProbeContractError::DisabledControlChanged { seed: pair.seed });
        }
    }

    let attempt_denominator = pairs
        .iter()
        .map(|pair| pair.attempts.attempted)
        .sum::<usize>();
    let attempt_denominator = std::num::NonZeroUsize::new(attempt_denominator)
        .ok_or(ProbeContractError::NoExchangeAttempts)?;
    let count = |select: fn(AttemptCounts) -> usize| {
        pairs
            .iter()
            .map(|pair| select(pair.attempts))
            .sum::<usize>()
    };

    let mut breaches = TreatmentOnlyBreaches::default();
    for pair in pairs {
        if !pair.control.breaches_settlement_count() && pair.treatment.breaches_settlement_count() {
            breaches.settlement_count += 1;
        }
        if !pair.control.breaches_collapse_share() && pair.treatment.breaches_collapse_share() {
            breaches.collapse_share += 1;
        }
        if !pair.control.breaches_alive_at_now() && pair.treatment.breaches_alive_at_now() {
            breaches.alive_at_now += 1;
        }
    }

    Ok(ProbeReport {
        world_denominator: pairs.len(),
        activation_worlds: pairs
            .iter()
            .filter(|pair| pair.attempts.settled > 0)
            .count(),
        attempted: attempt_denominator.get(),
        proposed: CountWithAttempts::new(count(|c| c.proposed), attempt_denominator),
        accepted: CountWithAttempts::new(count(|c| c.accepted), attempt_denominator),
        settled: CountWithAttempts::new(count(|c| c.settled), attempt_denominator),
        partial: CountWithAttempts::new(count(|c| c.partial), attempt_denominator),
        refused: CountWithAttempts::new(count(|c| c.refused), attempt_denominator),
        impossible: CountWithAttempts::new(count(|c| c.impossible), attempt_denominator),
        stock_conservation_residuals: pairs
            .iter()
            .map(|pair| StockConservationResiduals {
                seed: pair.seed,
                by_resource: pair.stock_conservation_residuals,
            })
            .collect(),
        treatment_only_breaches: breaches,
        byte_identical_disabled_controls: pairs
            .iter()
            .filter(|pair| pair.disabled_control_is_byte_identical)
            .count(),
    })
}

fn passing_reading() -> DemographicReading {
    DemographicReading {
        settlement_count: 40,
        collapse_share: MAX_COLLAPSE_SHARE,
        alive_at_now: MIN_ALIVE_AT_NOW,
    }
}

fn fixture_with_attempts(attempts: Vec<AttemptStatus>) -> Vec<PairObservation> {
    PROBE_SEEDS
        .map(|seed| PairObservation {
            seed,
            control: passing_reading(),
            treatment: passing_reading(),
            attempts: if seed == 1 {
                AttemptCounts::from_statuses(&attempts)
            } else {
                AttemptCounts::default()
            },
            stock_conservation_residuals: [0.0, 0.0],
            disabled_control_is_byte_identical: true,
        })
        .collect()
}

fn build_control_world(seed: u64, components: &WorldComponents) -> World {
    build_world_to(
        Seed(seed),
        &SkyPins::default(),
        &TerrainPins::default(),
        &SettlementPins::default(),
        components,
        BuildDepth::Settlements,
    )
    .expect("fixed probe seed builds to settlements")
}

/// The disabled treatment deliberately owns a builder boundary distinct from
/// control. Both boundaries invoke the unchanged history path and consume no
/// exchange state.
fn build_disabled_treatment_world(seed: u64, components: &WorldComponents) -> World {
    build_world_with_exchange_treatment(
        Seed(seed),
        &SkyPins::default(),
        &TerrainPins::default(),
        &SettlementPins::default(),
        components,
        ExchangeTreatment::Disabled,
    )
    .expect("fixed disabled-treatment seed builds to settlements")
    .world
}

fn integrated_pair(seed: u64, components: &WorldComponents) -> PairObservation {
    let control = build_control_world(seed, components);
    let disabled = build_world_with_exchange_treatment(
        Seed(seed),
        &SkyPins::default(),
        &TerrainPins::default(),
        &SettlementPins::default(),
        components,
        ExchangeTreatment::Disabled,
    )
    .expect("fixed disabled-treatment seed builds to settlements");
    let treatment = build_world_with_exchange_treatment(
        Seed(seed),
        &SkyPins::default(),
        &TerrainPins::default(),
        &SettlementPins::default(),
        components,
        ExchangeTreatment::Enabled,
    )
    .expect("fixed enabled-treatment seed builds to settlements");
    let control_bytes = serde_json::to_vec(&control.ledger).expect("control ledger serializes");
    let disabled_bytes =
        serde_json::to_vec(&disabled.world.ledger).expect("disabled ledger serializes");
    PairObservation {
        seed,
        control: DemographicReading::from_world(&control),
        treatment: DemographicReading::from_world(&treatment.world),
        attempts: AttemptCounts::from_census(treatment.exchange),
        stock_conservation_residuals: treatment.exchange.conservation_residuals,
        disabled_control_is_byte_identical: control_bytes == disabled_bytes,
    }
}

fn observe_disabled_pair(
    seed: u64,
    control: &World,
    disabled_treatment: &World,
) -> PairObservation {
    let control_bytes = serde_json::to_vec(&control.ledger).expect("control ledger serializes");
    let disabled_bytes = serde_json::to_vec(&disabled_treatment.ledger)
        .expect("disabled-treatment ledger serializes");
    PairObservation {
        seed,
        control: DemographicReading::from_world(control),
        treatment: DemographicReading::from_world(disabled_treatment),
        attempts: AttemptCounts::default(),
        stock_conservation_residuals: [0.0, 0.0],
        disabled_control_is_byte_identical: control_bytes == disabled_bytes,
    }
}

fn disabled_pair(seed: u64, components: &WorldComponents) -> PairObservation {
    let control = build_control_world(seed, components);
    let disabled_treatment = build_disabled_treatment_world(seed, components);
    observe_disabled_pair(seed, &control, &disabled_treatment)
}

#[test]
fn an_empty_outcome_set_is_not_misreported_as_zero_activation() {
    let pairs = fixture_with_attempts(Vec::new());
    assert_eq!(
        pairs.len(),
        PROBE_WORLD_DENOMINATOR,
        "the fixed fixture must contain the full world denominator"
    );
    assert!(
        pairs
            .iter()
            .all(|pair| pair.stock_conservation_residuals == [0.0, 0.0]),
        "the denominator check must execute after a conserving fixture"
    );
    assert_eq!(
        summarize_probe(&pairs),
        Err(ProbeContractError::NoExchangeAttempts),
        "zero attempts is an unavailable outcome denominator, not zero activation"
    );
}

/// claim: invariant(forall-seed in the fixed synthetic 200-world roster) —
/// every reported outcome carries the same real, non-zero attempt population.
#[test]
fn every_reported_outcome_has_its_own_non_zero_attempt_denominator() {
    let pairs = fixture_with_attempts(vec![
        AttemptStatus::Proposed,
        AttemptStatus::Accepted,
        AttemptStatus::Settled,
        AttemptStatus::Partial,
        AttemptStatus::Refused,
        AttemptStatus::Impossible,
    ]);
    let report = summarize_probe(&pairs).expect("all six outcomes produce a report");
    assert_eq!(report.world_denominator, PROBE_WORLD_DENOMINATOR);
    assert_eq!(report.activation_worlds, 1);
    assert_eq!(report.attempted, 6);
    assert_eq!(report.byte_identical_disabled_controls, 200);
    assert_eq!(report.stock_conservation_residuals.len(), 200);
    assert!(
        report
            .stock_conservation_residuals
            .iter()
            .all(|residual| residual.by_resource == [0.0, 0.0]),
        "the report must retain both conserving resource residuals for every seed"
    );
    for outcome in [
        report.proposed,
        report.accepted,
        report.settled,
        report.partial,
        report.refused,
        report.impossible,
    ] {
        assert_eq!(outcome.count, 1);
        assert_eq!(outcome.attempts.get(), 6);
    }
}

#[test]
fn zero_activation_with_attempts_is_distinct_from_zero_attempts() {
    let report = summarize_probe(&fixture_with_attempts(vec![
        AttemptStatus::Refused,
        AttemptStatus::Impossible,
    ]))
    .expect("failed attempts still have a real denominator");
    assert!(report.is_zero_activation());
    assert_eq!(report.refused.count, 1);
    assert_eq!(report.refused.attempts.get(), 2);
    assert_eq!(report.impossible.count, 1);
    assert_eq!(report.impossible.attempts.get(), 2);
}

#[test]
fn the_three_existing_bar_boundaries_are_inclusive_in_the_passing_direction() {
    for settlement_count in [40, 400] {
        let reading = DemographicReading {
            settlement_count,
            collapse_share: 0.05,
            alive_at_now: 50,
        };
        assert!(!reading.breaches_settlement_count());
        assert!(!reading.breaches_collapse_share());
        assert!(!reading.breaches_alive_at_now());
    }
}

/// claim: invariant(forall-seed, preregistered-bars) — treatment-only
/// instability counts only treatment breaches when the paired control passes;
/// settlement count passes inclusively inside `40..=400`, collapse passes at
/// or below `0.05`, and alive-at-now passes at or above `50`.
#[test]
fn demographic_instability_counts_only_treatment_only_bar_breaches() {
    let mut pairs = fixture_with_attempts(vec![AttemptStatus::Refused]);
    pairs[0].treatment.settlement_count = 39;
    pairs[1].control.settlement_count = 401;
    pairs[1].treatment.settlement_count = 401;
    pairs[2].treatment.collapse_share = MAX_COLLAPSE_SHARE + 0.001;
    pairs[3].control.collapse_share = MAX_COLLAPSE_SHARE + 0.001;
    pairs[3].treatment.collapse_share = MAX_COLLAPSE_SHARE + 0.002;
    pairs[4].treatment.alive_at_now = MIN_ALIVE_AT_NOW - 1;
    pairs[5].control.alive_at_now = MIN_ALIVE_AT_NOW - 1;
    pairs[5].treatment.alive_at_now = MIN_ALIVE_AT_NOW - 2;

    let report = summarize_probe(&pairs).expect("one failed attempt keeps the denominator live");
    assert_eq!(
        report.treatment_only_breaches,
        TreatmentOnlyBreaches {
            settlement_count: 1,
            collapse_share: 1,
            alive_at_now: 1,
        }
    );
    assert!(
        !report.treatment_only_breaches.crosses_instability_pole(),
        "one treatment-only breach of each bar is below the >100-world instability pole"
    );
}

/// claim: invariant(fixed-world-denominator, preregistered-direction) — the
/// instability pole is strictly more than half of the fixed 200-world
/// denominator, so 100 does not fire and 101 does.
#[test]
fn instability_requires_more_than_half_the_fixed_world_denominator() {
    let mut pairs = fixture_with_attempts(vec![AttemptStatus::Refused]);
    for pair in pairs.iter_mut().take(INSTABILITY_BREACH_FLOOR) {
        pair.treatment.settlement_count = 39;
    }
    let half = summarize_probe(&pairs).expect("the fixed roster has an attempt denominator");
    assert!(
        !half.treatment_only_breaches.crosses_instability_pole(),
        "exactly 100 treatment-only breaches is not the >100 instability pole"
    );

    pairs[INSTABILITY_BREACH_FLOOR].treatment.settlement_count = 39;
    let majority = summarize_probe(&pairs).expect("the fixed roster has an attempt denominator");
    assert!(
        majority.treatment_only_breaches.crosses_instability_pole(),
        "101 treatment-only breaches must cross the >100 instability pole"
    );
}

#[test]
fn a_disabled_treatment_difference_is_rejected_before_zero_attempts() {
    let components = WorldComponents::assemble().expect("canonical components assemble");
    let control = build_control_world(11, &components);
    let mut disabled_treatment = build_disabled_treatment_world(11, &components);
    disabled_treatment
        .ledger
        .mint_entity(test_lineage(u16::MAX));

    let mut pairs = fixture_with_attempts(Vec::new());
    pairs[10] = observe_disabled_pair(11, &control, &disabled_treatment);
    assert_eq!(
        summarize_probe(&pairs),
        Err(ProbeContractError::DisabledControlChanged { seed: 11 })
    );
}

/// claim: invariant(forall-seed, disabled-control) — the explicit disabled
/// treatment emits the same ledger bytes as the untouched control and emits
/// no exchange census. The assertion compares two independently constructed
/// boundaries, not a value with itself.
#[test]
fn explicit_disabled_exchange_treatment_is_control_identical() {
    let components = WorldComponents::assemble().expect("canonical components assemble");
    let control = build_control_world(11, &components);
    let disabled = build_world_with_exchange_treatment(
        Seed(11),
        &SkyPins::default(),
        &TerrainPins::default(),
        &SettlementPins::default(),
        &components,
        ExchangeTreatment::Disabled,
    )
    .expect("focused disabled-treatment world builds");

    assert_eq!(
        serde_json::to_vec(&disabled.world.ledger).unwrap(),
        serde_json::to_vec(&control.ledger).unwrap(),
        "the explicit disabled boundary must preserve current build bytes"
    );
    assert_eq!(
        disabled.exchange,
        ExchangeCensus::default(),
        "the disabled boundary must not fabricate an exchange trace"
    );
}

#[test]
fn non_finite_stock_residuals_are_rejected_before_outcome_reporting() {
    let mut pairs = fixture_with_attempts(Vec::new());
    pairs[17].stock_conservation_residuals[1] = f64::NAN;
    assert_eq!(
        summarize_probe(&pairs),
        Err(ProbeContractError::NonFiniteStockResidual {
            seed: 18,
            resource_index: 1,
        })
    );
}

#[test]
fn finite_nonzero_stock_residuals_are_rejected_as_non_conserving() {
    let mut pairs = fixture_with_attempts(vec![AttemptStatus::Refused]);
    pairs[22].stock_conservation_residuals[0] = 0.25;
    assert_eq!(
        summarize_probe(&pairs),
        Err(ProbeContractError::NonZeroStockResidual {
            seed: 23,
            resource_index: 0,
        }),
        "a finite residual still represents stock creation or destruction"
    );
}

/// claim: invariant(forall-seed, off-gate, probe:) — for the fixed 200-seed
/// preregistered roster, constructing the disabled treatment through its separate
/// fixture boundary emits the same ledger bytes as the untouched control.
#[test]
#[ignore = "probe: 200 paired settlement builds for The Staple D2 disabled-control identity"]
fn fixed_200_seed_disabled_control_is_byte_identical() {
    let components = WorldComponents::assemble().expect("canonical components assemble");
    let pairs: Vec<_> = PROBE_SEEDS
        .map(|seed| disabled_pair(seed, &components))
        .collect();
    assert!(
        pairs
            .iter()
            .all(|pair| pair.disabled_control_is_byte_identical),
        "every fixed-seed disabled treatment must be byte-identical to its same-seed control"
    );
    assert_eq!(
        summarize_probe(&pairs),
        Err(ProbeContractError::NoExchangeAttempts),
        "the pre-production 200-world fixture must validate identity and conservation before reporting zero attempts"
    );
}

/// claim: readout(forall-seed in 1..=200, off-gate) — the integrated D2
/// treatment activates on real worlds while conserving typed stock, and no
/// individually named demographic bar crosses the preregistered >100/200
/// instability pole. This test is intentionally run once per Task 4.
#[test]
#[ignore = "probe: one integrated 200-seed D2 control/treatment report; run exactly once"]
fn integrated_200_seed_exchange_treatment_report() {
    let components = WorldComponents::assemble().expect("canonical components assemble");
    let pairs: Vec<_> = PROBE_SEEDS
        .map(|seed| integrated_pair(seed, &components))
        .collect();
    let report = summarize_probe(&pairs).expect("integrated treatment has a real attempt set");

    println!("D2 integrated paired report");
    println!("worlds={}", report.world_denominator);
    println!(
        "disabled_control_identity={}",
        report.byte_identical_disabled_controls
    );
    println!(
        "activation={}/{}",
        report.activation_worlds, report.world_denominator
    );
    println!("attempted={}", report.attempted);
    println!(
        "proposed={}/{}",
        report.proposed.count, report.proposed.attempts
    );
    println!(
        "accepted={}/{}",
        report.accepted.count, report.accepted.attempts
    );
    println!(
        "settled={}/{}",
        report.settled.count, report.settled.attempts
    );
    println!(
        "partial={}/{}",
        report.partial.count, report.partial.attempts
    );
    println!(
        "refused={}/{}",
        report.refused.count, report.refused.attempts
    );
    println!(
        "impossible={}/{}",
        report.impossible.count, report.impossible.attempts
    );
    let nonzero_residual_worlds = report
        .stock_conservation_residuals
        .iter()
        .filter(|residual| residual.by_resource != [0.0, 0.0])
        .count();
    println!(
        "stock_conservation_residuals nonzero_worlds={}/{}",
        nonzero_residual_worlds, report.world_denominator
    );
    println!(
        "treatment_only_breaches settlement_count={}/{} collapse_share={}/{} alive_at_now={}/{}",
        report.treatment_only_breaches.settlement_count,
        report.world_denominator,
        report.treatment_only_breaches.collapse_share,
        report.world_denominator,
        report.treatment_only_breaches.alive_at_now,
        report.world_denominator,
    );
    println!("zero_activation={}", report.is_zero_activation());
    println!(
        "crosses_instability_pole={}",
        report.treatment_only_breaches.crosses_instability_pole()
    );

    assert_eq!(report.world_denominator, PROBE_WORLD_DENOMINATOR);
    assert_eq!(
        report.byte_identical_disabled_controls,
        PROBE_WORLD_DENOMINATOR
    );
    assert!(
        report.attempted > 0,
        "the treatment must exercise a real attempt denominator"
    );
    assert_eq!(
        report.stock_conservation_residuals.len(),
        PROBE_WORLD_DENOMINATOR
    );
}
