//! The Diachronic Book (C8 Task 1): the observation ledger and the
//! knowledge ladder — the accumulation law, the witness law, the ladder
//! law (the full measured rung table, pinned exact), and the prophecy
//! law, all measured against live seeds.

use hornvale_astronomy::{EclipseBody, StdDays};
use hornvale_worldgen::{
    LadderRung, SettlementPins, SkyChoice, doctrine_of, ladder_of, observations_of, placed_peoples,
};

/// Build a world with the shipped four-people component set, generated
/// sky, default terrain/settlement pins — the shared pattern every
/// neighboring worldgen integration test (`doctrine.rs`,
/// `explanations.rs`, `chorus_params.rs`) uses.
fn generated(seed: u64) -> hornvale_kernel::World {
    hornvale_worldgen::build_world(
        hornvale_kernel::Seed(seed),
        &hornvale_astronomy::SkyPins::default(),
        SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &SettlementPins::default(),
    )
    .unwrap()
}

/// The preregistered epoch pair (plan header): day 0 and the hundredth
/// year.
const EPOCH_1: f64 = 0.0;
const EPOCH_2: f64 = 36_525.0;

fn at(day: f64) -> StdDays {
    StdDays::new(day).unwrap()
}

#[test]
fn observations_at_day_zero_are_empty() {
    // Every placed culture, seeds 1..=3: nothing has happened by day 0,
    // so the witnessed set is empty and the rung is Unknown.
    for seed in 1..=3u64 {
        let w = generated(seed);
        let placed = placed_peoples(&w);
        assert!(!placed.is_empty(), "seed {seed} must place cultures");
        for (kind, _) in placed {
            let obs = observations_of(&w, kind, at(EPOCH_1)).unwrap();
            assert!(
                obs.events.is_empty(),
                "seed {seed} {kind}: observations at day 0 must be empty, got {}",
                obs.events.len()
            );
            let (rung, prediction) = ladder_of(&w, kind, at(EPOCH_1)).unwrap();
            assert_eq!(rung, LadderRung::Unknown, "seed {seed} {kind} at day 0");
            assert_eq!(prediction, None, "no prediction below Predictive");
        }
    }
}

#[test]
fn the_accumulation_law() {
    // Per culture, seeds 1..=3, over {0, 10_000, 36_525}: |observations|
    // is monotone in T, and (events ascending by day) the earlier set is
    // a PREFIX of the later — more time never rewrites the record, only
    // extends it.
    for seed in 1..=3u64 {
        let w = generated(seed);
        for (kind, _) in placed_peoples(&w) {
            let t0 = observations_of(&w, kind, at(0.0)).unwrap();
            let t1 = observations_of(&w, kind, at(10_000.0)).unwrap();
            let t2 = observations_of(&w, kind, at(36_525.0)).unwrap();
            assert!(t0.events.len() <= t1.events.len(), "seed {seed} {kind}");
            assert!(t1.events.len() <= t2.events.len(), "seed {seed} {kind}");
            assert!(
                t1.events.starts_with(&t0.events),
                "seed {seed} {kind}: T=0 set must prefix T=10000"
            );
            assert!(
                t2.events.starts_with(&t1.events),
                "seed {seed} {kind}: T=10000 set must prefix T=36525"
            );
            // Ascending by day (the order the scan promises).
            for pair in t2.events.windows(2) {
                assert!(
                    pair[0].0 <= pair[1].0,
                    "seed {seed} {kind}: events must ascend by day"
                );
            }
        }
    }
}

#[test]
fn the_witness_law() {
    // Seed 2 (measured): goblin capability 0.5, hobgoblin 0.55, kobold
    // 1.0 — the roster straddles the 0.6 lunar threshold on ONE world.
    // Solar events are public: every culture's solar subset is IDENTICAL
    // regardless of capability. Lunar events are night-gated: goblin and
    // hobgoblin (below 0.6) witness none; kobold (1.0) witnesses them.
    let w = generated(2);
    let t = at(EPOCH_2);

    let solar_of = |kind: &str| -> Vec<(f64, usize, EclipseBody)> {
        observations_of(&w, kind, t)
            .unwrap()
            .events
            .into_iter()
            .filter(|(_, _, b)| *b == EclipseBody::Solar)
            .collect()
    };
    let lunar_of = |kind: &str| -> Vec<(f64, usize, EclipseBody)> {
        observations_of(&w, kind, t)
            .unwrap()
            .events
            .into_iter()
            .filter(|(_, _, b)| *b == EclipseBody::Lunar)
            .collect()
    };

    // Low-cap (goblin 0.5) vs high-cap (kobold 1.0): solar subsets equal.
    let goblin_solar = solar_of("goblin");
    let kobold_solar = solar_of("kobold");
    assert!(!goblin_solar.is_empty(), "seed 2 must have solar events");
    assert_eq!(
        goblin_solar, kobold_solar,
        "solar witnessing must not depend on sky-capability"
    );
    assert_eq!(goblin_solar, solar_of("hobgoblin"));
    assert_eq!(goblin_solar.len(), 49, "measured seed 2 solar count");

    // Lunar subsets differ exactly where capability straddles 0.6.
    assert!(
        lunar_of("goblin").is_empty(),
        "goblin (capability 0.5 < 0.6) must witness no lunar events"
    );
    assert!(
        lunar_of("hobgoblin").is_empty(),
        "hobgoblin (capability 0.55 < 0.6) must witness no lunar events"
    );
    let kobold_lunar = lunar_of("kobold");
    assert_eq!(
        kobold_lunar.len(),
        32,
        "MEASURED live arm: seed 2 HAS lunar events by day 36525 and the kobold (capability 1.0) \
         witnesses exactly 32 of them. If a physics change ever empties this set, do NOT weaken \
         this assertion — run a wider seed sweep to find a world with lunar events and re-pin, so \
         the lunar arm of the witness law never passes vacuously."
    );
}

/// One pinned rung-table row: seed, culture, epoch-1 rung, epoch-2 rung,
/// epoch-2 witnessed count, epoch-2 prediction (day, exact f64).
type Row = (
    u64,
    &'static str,
    LadderRung,
    LadderRung,
    usize,
    Option<f64>,
);

/// The FULL measured ladder table (seeds 1..=5 × placed culture × the
/// epoch pair), pinned exact — the C7 landscape idiom. Predictions are
/// the exact f64 the API returned (shortest-round-trip literals).
// Re-pinned under The Living Community epoch (history is the sole
// settlement placer, this merge): the deep-history bake seeds all four
// peoples on every world and grows them into large, organized flagship
// communities. Every placed culture now clears the SOC-1 gate (has
// doctrine), so every epoch-2 rung is Predictive - the prior folk-only
// rows (seed 1 hobgoblin, seed 2 kobold, seed 5 hobgoblin) are gone, and
// bugbear/kobold now place at every seed. The epoch-2 prediction is a
// closed-form function of the world's day, so a people's co-seeded twin
// flagships share it exactly.
const LADDER_TABLE: &[Row] = &[
    (
        1,
        "bugbear",
        LadderRung::Unknown,
        LadderRung::Predictive,
        6472,
        Some(36526.1181181615),
    ),
    (
        1,
        "goblin",
        LadderRung::Unknown,
        LadderRung::Predictive,
        4010,
        Some(36526.1181181615),
    ),
    (
        1,
        "hobgoblin",
        LadderRung::Unknown,
        LadderRung::Predictive,
        4010,
        Some(36526.1181181615),
    ),
    (
        1,
        "kobold",
        LadderRung::Unknown,
        LadderRung::Predictive,
        6472,
        Some(36526.1181181615),
    ),
    (
        2,
        "bugbear",
        LadderRung::Unknown,
        LadderRung::Predictive,
        81,
        Some(36611.93610859729),
    ),
    (
        2,
        "goblin",
        LadderRung::Unknown,
        LadderRung::Predictive,
        49,
        Some(36611.93610859729),
    ),
    (
        2,
        "hobgoblin",
        LadderRung::Unknown,
        LadderRung::Predictive,
        49,
        Some(36611.93610859729),
    ),
    (
        2,
        "kobold",
        LadderRung::Unknown,
        LadderRung::Predictive,
        81,
        Some(36611.93610859729),
    ),
    (
        3,
        "bugbear",
        LadderRung::Unknown,
        LadderRung::Predictive,
        53,
        Some(36953.336612553074),
    ),
    (
        3,
        "goblin",
        LadderRung::Unknown,
        LadderRung::Predictive,
        32,
        Some(36953.336612553074),
    ),
    (
        3,
        "hobgoblin",
        LadderRung::Unknown,
        LadderRung::Predictive,
        32,
        Some(36953.336612553074),
    ),
    (
        3,
        "kobold",
        LadderRung::Unknown,
        LadderRung::Predictive,
        53,
        Some(36953.336612553074),
    ),
    (
        4,
        "bugbear",
        LadderRung::Unknown,
        LadderRung::Predictive,
        3785,
        Some(36540.36159622378),
    ),
    (
        4,
        "goblin",
        LadderRung::Unknown,
        LadderRung::Predictive,
        2067,
        Some(36540.36159622378),
    ),
    (
        4,
        "hobgoblin",
        LadderRung::Unknown,
        LadderRung::Predictive,
        2067,
        Some(36540.36159622378),
    ),
    (
        4,
        "kobold",
        LadderRung::Unknown,
        LadderRung::Predictive,
        3785,
        Some(36540.36159622378),
    ),
    (
        5,
        "bugbear",
        LadderRung::Unknown,
        LadderRung::Predictive,
        500,
        Some(36550.4595587471),
    ),
    (
        5,
        "goblin",
        LadderRung::Unknown,
        LadderRung::Predictive,
        304,
        Some(36550.4595587471),
    ),
    (
        5,
        "hobgoblin",
        LadderRung::Unknown,
        LadderRung::Predictive,
        304,
        Some(36550.4595587471),
    ),
    (
        5,
        "kobold",
        LadderRung::Unknown,
        LadderRung::Predictive,
        500,
        Some(36550.4595587471),
    ),
];

#[test]
fn the_ladder_law() {
    // The full measured (seed 1..=5 × culture × epoch) rung table, pinned
    // exact, plus the structural law: no rung above Counted without
    // doctrine_of (the SOC-1 gate's diachronic consequence).
    for seed in 1..=5u64 {
        let w = generated(seed);
        let placed = placed_peoples(&w);
        let expected_rows: Vec<&Row> = LADDER_TABLE.iter().filter(|r| r.0 == seed).collect();
        assert_eq!(
            placed.len(),
            expected_rows.len(),
            "seed {seed}: the pinned table must cover every placed culture"
        );
        for (kind, _) in placed {
            let row = LADDER_TABLE
                .iter()
                .find(|r| r.0 == seed && r.1 == kind)
                .unwrap_or_else(|| panic!("seed {seed} {kind}: missing from the pinned table"));
            let (rung_1, pred_1) = ladder_of(&w, kind, at(EPOCH_1)).unwrap();
            let (rung_2, pred_2) = ladder_of(&w, kind, at(EPOCH_2)).unwrap();
            let n_2 = observations_of(&w, kind, at(EPOCH_2)).unwrap().events.len();
            assert_eq!(rung_1, row.2, "seed {seed} {kind} epoch 1 rung");
            assert_eq!(pred_1, None, "seed {seed} {kind}: no epoch-1 prediction");
            assert_eq!(rung_2, row.3, "seed {seed} {kind} epoch 2 rung");
            assert_eq!(n_2, row.4, "seed {seed} {kind} epoch 2 witnessed count");
            assert_eq!(pred_2, row.5, "seed {seed} {kind} epoch 2 prediction");

            // Structural: no Numbered (or higher) without doctrine.
            let organized = doctrine_of(&w, kind).is_some();
            for rung in [rung_1, rung_2] {
                if matches!(rung, LadderRung::Numbered | LadderRung::Predictive) {
                    assert!(
                        organized,
                        "seed {seed} {kind}: rung {rung:?} without an organized cult"
                    );
                }
            }
            if !organized {
                assert!(
                    matches!(rung_2, LadderRung::Unknown | LadderRung::Counted),
                    "seed {seed} {kind}: folk-only cultures never exceed Counted, got {rung_2:?}"
                );
            }
        }
    }

    // The full four-rung climb on one culture (measured: seed 3 goblin,
    // the sparsest organized culture — 32 events/century), so the
    // Numbered rung — which both committed epochs skip over — is
    // exercised live, never vacuously.
    let w = generated(3);
    let climb = [
        (1_000.0, LadderRung::Unknown, None),
        (2_000.0, LadderRung::Counted, None),
        (4_000.0, LadderRung::Numbered, None),
        (8_000.0, LadderRung::Predictive, Some(9080.42957840976)),
    ];
    for (day, expected_rung, expected_pred) in climb {
        let (rung, pred) = ladder_of(&w, "goblin", at(day)).unwrap();
        assert_eq!(rung, expected_rung, "seed 3 goblin at day {day}");
        assert_eq!(pred, expected_pred, "seed 3 goblin at day {day}");
    }
}

#[test]
fn the_prophecy_law() {
    // Every Predictive culture's predicted day D: the closed-form future
    // scan contains an event of the predicted class at EXACTLY D — the
    // f64s the API returns compared directly (same computation, no
    // epsilon; quantization happens at emit boundaries, never here).
    // PANIC if nobody in seeds 1..=5 reaches Predictive at epoch 2 —
    // the ladder's top must be visible (the preregistered demand).
    const HORIZON: f64 = 10_000.0;
    let t = at(EPOCH_2);
    let mut any_predictive = false;

    for seed in 1..=5u64 {
        let w = generated(seed);
        for (kind, _) in placed_peoples(&w) {
            let (rung, prediction) = ladder_of(&w, kind, t).unwrap();
            if rung != LadderRung::Predictive {
                continue;
            }
            any_predictive = true;
            let Some(day) = prediction else {
                // The honest horizon arm: Predictive with no event inside
                // the teaching horizon. Legal, but unmeasured at these
                // seeds — every measured Predictive culture predicts.
                panic!(
                    "seed {seed} {kind}: Predictive with prediction None was never a measured \
                     outcome at these seeds — if this fires, a physics change moved the event \
                     cadence; re-measure and re-pin"
                );
            };
            assert!(
                day > EPOCH_2,
                "seed {seed} {kind}: the prediction is future"
            );

            // Re-derive the most-observed recurrence class with the same
            // deterministic tie-break ladder_of documents (max count,
            // ties toward the numerically smallest (moon, body) key).
            let obs = observations_of(&w, kind, t).unwrap();
            let mut counts: std::collections::BTreeMap<(usize, u8), usize> =
                std::collections::BTreeMap::new();
            for &(_, moon, body) in &obs.events {
                let d = match body {
                    EclipseBody::Solar => 0u8,
                    EclipseBody::Lunar => 1u8,
                };
                *counts.entry((moon, d)).or_insert(0) += 1;
            }
            let (&(moon, body_d), _) = counts
                .iter()
                .max_by_key(|(key, count)| (**count, std::cmp::Reverse(**key)))
                .expect("a Predictive culture has witnessed events");
            let body = if body_d == 0 {
                EclipseBody::Solar
            } else {
                EclipseBody::Lunar
            };

            // The future event set must contain the predicted event at
            // exactly D, of exactly the predicted class.
            let sky = hornvale_worldgen::sky_of(&w).unwrap();
            let hornvale_worldgen::Sky::Generated(sky) = sky else {
                panic!("generated worlds carry a Generated sky");
            };
            let future = hornvale_astronomy::eclipse_events(
                sky.system(),
                sky.calendar(),
                t,
                at(EPOCH_2 + HORIZON),
            );
            assert!(
                future
                    .iter()
                    .any(|e| e.day.get() == day && e.moon == moon && e.body == body),
                "seed {seed} {kind}: predicted day {day} of class ({moon}, {body:?}) must exist \
                 in the closed-form future event set — doctrine's predictions are never wrong"
            );
        }
    }

    assert!(
        any_predictive,
        "NO culture in seeds 1..=5 reaches Predictive at epoch 2 — the ladder's top must be \
         visible somewhere (the preregistered demand); widen the epoch or seed sweep"
    );
}

#[test]
fn diachronic_is_deterministic() {
    // Pure derivation: the same (world, species, at) twice → identical
    // Debug output, for both the ledger and the ladder.
    let w = generated(1);
    let t = at(EPOCH_2);
    for (kind, _) in placed_peoples(&w) {
        let obs_a = format!("{:?}", observations_of(&w, kind, t).unwrap());
        let obs_b = format!("{:?}", observations_of(&w, kind, t).unwrap());
        assert_eq!(obs_a, obs_b, "{kind}: observations_of must be pure");
        let ladder_a = format!("{:?}", ladder_of(&w, kind, t).unwrap());
        let ladder_b = format!("{:?}", ladder_of(&w, kind, t).unwrap());
        assert_eq!(ladder_a, ladder_b, "{kind}: ladder_of must be pure");
    }
}
