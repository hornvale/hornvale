//! LANG-48 property tests: the five standing laws from
//! `docs/superpowers/specs/2026-07-20-the-consonance-design.md` §4,
//! measured directly — never narrated.

use hornvale_astronomy::StdDays;
use hornvale_astronomy::resonance::detect_moon_period_ratio;
use hornvale_language::numeracy::{NumeracyRung, expressible_at_rung};
use hornvale_language::schemas::SchemaId;
use hornvale_language::{Disposition, LossReason};
use hornvale_worldgen::{SettlementPins, SkyChoice, accounts_of};

fn days(x: f64) -> StdDays {
    StdDays::new(x).expect("test fixture uses a finite positive value")
}

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

#[test]
fn purity_same_inputs_same_output() {
    let periods = [days(30.0), days(60.0)];
    assert_eq!(
        detect_moon_period_ratio(&periods),
        detect_moon_period_ratio(&periods)
    );
    assert_eq!(
        expressible_at_rung(2, 1, NumeracyRung::FullCounting),
        expressible_at_rung(2, 1, NumeracyRung::FullCounting)
    );
}

#[test]
fn non_degeneracy_some_worlds_match_some_dont() {
    let matching = [days(30.0), days(60.0)];
    let non_matching = [days(30.0), days(55.0)];
    assert!(detect_moon_period_ratio(&matching).is_some());
    assert!(detect_moon_period_ratio(&non_matching).is_none());
}

/// Law 3 + Law 4, together, over real generated worlds and the full
/// public `accounts_of` pipeline: search a fixed seed range for a world
/// where astronomy's own real moon periods produced a clean ratio, then
/// confirm that for every placed culture, `moon-period-ratio` is EITHER
/// absent from the account entirely, OR present and gated below this
/// culture's own sky-capability threshold (`Lost(BeyondCapability {
/// domain: "sky" })` — a real, honest outcome; `explain_moon_ratio` in
/// `windows/worldgen/src/chorus.rs` only ever fires on an entry whose
/// disposition is exactly `Kept`, so a below-threshold entry is left
/// exactly here, never nudged toward `Explained`), OR present and
/// `Explained` with a schema in the real `FactShape::CyclicEvent`
/// admitted set (Task 4's own unit test already proves this mechanism
/// directly) — never anything else (never a bare `Kept` left unexplained
/// when a real pantheon exists to bind against).
///
/// (The brief that seeded this test originally modeled the below-
/// capability outcome as an absent account entry; the real
/// `account_of`/`explain_moon_ratio` pipeline instead always produces an
/// entry for every ground fact and represents "below capability" via
/// `Lost(BeyondCapability)`, so this test's match arms were corrected to
/// the mechanism actually observed over these seeds.)
///
/// The admitted set for `FactShape::CyclicEvent` is FOUR schemas, read
/// directly from `domains/language/src/schemas.rs`'s `schema_table()`:
/// Agentive, PathJourney, Balance, and CycleReturn (see also Task 4's own
/// `moon_period_ratio_is_explained_with_an_admitted_schema` test in
/// `windows/worldgen/src/chorus.rs`, which asserts the identical
/// four-member set).
#[test]
fn witnessed_access_and_explanation_hold_over_a_real_world() {
    let mut checked_at_least_one_culture = false;
    for seed in 1u64..=50 {
        let outcome = hornvale_astronomy::system::generate(
            hornvale_kernel::Seed(seed),
            &hornvale_astronomy::SkyPins::default(),
        )
        .unwrap();
        let periods: Vec<_> = outcome.system.moons.iter().map(|m| m.period).collect();
        if detect_moon_period_ratio(&periods).is_none() {
            continue;
        }
        let world = generated(seed);
        let voices = accounts_of(&world);
        for voice in &voices {
            let Some(entry) = voice
                .account
                .entries
                .iter()
                .find(|e| e.fact.predicate == hornvale_astronomy::facts::MOON_PERIOD_RATIO)
            else {
                checked_at_least_one_culture = true;
                continue;
            };
            match &entry.disposition {
                Disposition::Explained { schema, .. } => {
                    assert!(
                        matches!(
                            schema,
                            SchemaId::Agentive
                                | SchemaId::PathJourney
                                | SchemaId::Balance
                                | SchemaId::CycleReturn
                        ),
                        "seed {seed}, {}: fired schema {schema:?} must be in the real admitted set",
                        voice.kind
                    );
                }
                Disposition::Lost(LossReason::BeyondCapability { domain: "sky" }) => {
                    // Below this culture's own sky-capability threshold —
                    // the honest, expected outcome; `explain_moon_ratio`
                    // never touches an entry that isn't `Kept`.
                }
                other => panic!(
                    "seed {seed}, {}: moon-period-ratio entry must be Explained or gated \
                     Lost(BeyondCapability {{ domain: \"sky\" }}), got {other:?}",
                    voice.kind
                ),
            }
            checked_at_least_one_culture = true;
        }
        if checked_at_least_one_culture {
            return;
        }
    }
    panic!("no seed in 1..=50 both produced a clean moon-period ratio and placed a culture");
}

#[test]
fn expressibility_tracks_the_rung_honestly() {
    let ratio =
        detect_moon_period_ratio(&[days(30.0), days(60.0)]).expect("30/60 is a clean 2:1 fixture");
    assert!(!expressible_at_rung(
        ratio.numerator,
        ratio.denominator,
        NumeracyRung::Subitizing
    ));
    assert!(expressible_at_rung(
        ratio.numerator,
        ratio.denominator,
        NumeracyRung::FullCounting
    ));
    assert!(expressible_at_rung(
        ratio.numerator,
        ratio.denominator,
        NumeracyRung::Decimals
    ));
}
