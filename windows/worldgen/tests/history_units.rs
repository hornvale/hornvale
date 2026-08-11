//! The ledger's time axis is days. The history bake reasons in years and
//! converts at its emit boundary; nothing downstream should ever see a year in
//! a `Fact.day` stamp or in an `occ-founded`/`occ-ended`/`history-now` object.
//!
//! These are live-world tests on purpose. The defect The Ell repairs survived
//! every hand-built unit test in the tree, because a hand-built record has
//! whatever unit its author typed into it — only a real bake, committed to a
//! real ledger and read back by a real consumer, can disagree with itself.

use hornvale_kernel::{CellId, EntityId, Seed, Value};
use hornvale_worldgen::{
    BuildDepth, SettlementPins, SkyChoice, WorldComponents, build_world, build_world_to,
    occupation_records,
};

/// The same helper `windows/worldgen/tests/person_promotion.rs` uses — the
/// witness seed, built to full depth.
fn world() -> hornvale_kernel::World {
    build_world(
        Seed(42),
        &hornvale_astronomy::SkyPins::default(),
        SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &SettlementPins::default(),
    )
    .expect("seed 42 builds")
}

/// The campaign's behavioural red: a founder in a two-millennium history must
/// be able to die. `person-died` is uncommittable while promotion subtracts a
/// maturity in DAYS from a founding day in YEARS and then compares the sum
/// against a present in years.
#[test]
fn a_promoted_founder_can_die() {
    let world = world();
    let deaths = world.ledger.find(hornvale_person::PERSON_DIED).count();
    let people = world.ledger.find(hornvale_person::IS_PERSON).count();
    assert!(people > 0, "the witness seed must promote founders at all");
    assert!(
        deaths > 0,
        "no founder has died in a 2000-year history — the death gate is \
         comparing a day against a year"
    );
    assert!(
        deaths < people,
        "every founder died, which is as wrong as none dying: {deaths} of {people}"
    );
}

/// The unit crossing itself, stated as a number rather than as a consequence.
///
/// `BakeConfig::default_millennia` closes at `end_year = 2000.0`; the ledger
/// speaks standard days, so the committed present is 2000 Julian years of
/// them. Drop the conversion in `history_emit::emit_now` and this reads 2000.0
/// — the value the almanac then renders as "the year 2000" by dividing it back
/// out, which is why the defect was invisible from the prose end.
#[test]
fn the_committed_present_is_a_span_of_days_not_years() {
    let world = world();
    let now = world
        .ledger
        .find(hornvale_history::HISTORY_NOW)
        .next()
        .and_then(|f| match f.object {
            Value::Number(n) => Some(n),
            _ => None,
        })
        .expect("a fully built world commits its present");
    assert_eq!(
        now,
        2000.0 * hornvale_kernel::Years::DAYS_PER_YEAR,
        "history-now must be the bake's 2000-year span expressed in standard \
         days, not the bare year count"
    );
}

/// Every occupation fact's day stamp is the same day its object describes, and
/// both are days.
///
/// The stamp and the object are converted at two separate call sites, so this
/// is the guard that catches converting one and not the other — a state in
/// which every other test in the tree still passes.
#[test]
fn a_founding_fact_is_stamped_on_the_day_it_records() {
    let world = world();
    let mut checked = 0usize;
    for f in world.ledger.find(hornvale_history::OCC_FOUNDED) {
        let Value::Number(founded) = f.object else {
            panic!("occ-founded carries a number");
        };
        let stamp = f.day.expect("an occupation fact is dated").day();
        assert_eq!(
            founded, stamp,
            "occ-founded's object and its own Fact.day stamp must be the same \
             day in the same unit"
        );
        checked += 1;
    }
    assert!(checked > 0, "seed 42 must bake some occupations");
}

/// The disposition draw's two paths still agree — measured on a **real world**,
/// not on a hand-built ledger.
///
/// `tolerance_draw.rs`'s `the_bake_side_key_path_and_the_ledger_side_wrapper_agree`
/// already pins this contract and is the sharper test of the arithmetic, but it
/// builds its own synthetic settlement, so it can only ever be as right about
/// the committed unit as its fixture is. This one takes the ledger a real bake
/// wrote. It is the guard on the most dangerous conversion in the campaign:
/// `settlement_disposition` reduces `occ-founded` into a **frozen leg-string**
/// inside a seed derivation (`"{site}/{founded_year}"`), so getting the unit
/// wrong there gives every settlement in every world a different mind with
/// nothing red anywhere.
///
/// Proven by mutation: dropping the `bake_year_of_ledger_day` call in
/// `settlement_disposition` fails this and its synthetic sibling.
#[test]
fn the_two_disposition_paths_agree_on_a_real_world() {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    let world = build_world_to(
        Seed(42),
        &hornvale_astronomy::SkyPins::default(),
        SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &SettlementPins::default(),
        &wc,
        // Settlements is the shallowest rung that bakes and commits a history.
        BuildDepth::Settlements,
    )
    .expect("seed 42 builds");

    let psyche = hornvale_species::psyche_registry();
    let dispersion = hornvale_species::dispersion_registry();
    let mut checked = 0usize;
    for record in occupation_records(&world) {
        // The wrapper is defined only on ALIVE settlements (a ruin commits no
        // `cell-id`), which is the population `tolerance_draw.rs` documents.
        let id: EntityId = record.id;
        let Some(via_ledger) = hornvale_worldgen::disposition::settlement_disposition(&world, id)
        else {
            continue;
        };
        // The bake side never touches the ledger: it holds the record's own
        // founding year and people label.
        let via_key = hornvale_worldgen::disposition::people_disposition(
            world.seed,
            CellId(record.core.site.0),
            hornvale_worldgen::disposition::occupation_draw_key(record.core.founded),
            record.core.people.0,
            &psyche,
            &dispersion,
        )
        .expect("a placed people carries a mind");
        assert_eq!(
            via_ledger, via_key,
            "bake-side and ledger-side disagree for {} at site {:?} founded \
             year {}: the world would REPORT a disposition its history was not \
             BAKED with",
            record.core.people.0, record.core.site, record.core.founded
        );
        checked += 1;
    }
    assert!(
        checked > 0,
        "seed 42 must stand some settlements, or this test asserts nothing"
    );
}

/// The days→years read is lossless for every world this project builds.
///
/// `Ledger::commit` quantizes objects to 8 significant digits, so the
/// round trip year → day → quantize → year is exact only while the bake's
/// foundings stay coarse enough to survive it. They do: `epoch_years = 25`, so
/// every founding is a whole multiple of 25 years and its day form
/// (`k · 9131.25`) needs at most 8 significant digits. This asserts the
/// property rather than the arithmetic behind it — if a future bake steps in
/// finer units, every founder handle and every flesh seed in the world moves
/// silently, and this is what says so out loud.
#[test]
fn reading_a_founding_back_out_of_the_ledger_is_lossless() {
    let world = world();
    let records = occupation_records(&world);
    assert!(!records.is_empty(), "seed 42 must bake some occupations");
    for r in &records {
        assert_eq!(
            r.core.founded,
            r.core.founded.round(),
            "a founding read back off the ledger must still be the whole year \
             the bake wrote: {}",
            r.core.founded
        );
        if let Some(ended) = r.core.ended {
            assert_eq!(
                ended,
                ended.round(),
                "an ending read back off the ledger must still be the whole \
                 year the bake wrote: {ended}"
            );
        }
    }
}
