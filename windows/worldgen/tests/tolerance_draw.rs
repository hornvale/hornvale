//! The Tolerance, Task 3: the per-settlement disposition draw.
//!
//! ## What the draw is keyed on, and why it is not the obvious thing
//!
//! A settlement's drawn mind must be a function of a **stable semantic
//! identity** — spec decision D3, itself a consequence of The Salt's ratified
//! rule that an `EntityId` may be stored, compared and looked up but never
//! *read for its value*. Three candidate keys were considered and two
//! rejected:
//!
//! - **`EntityId` — rejected (D3).** `Ledger::mint_entity` assigns
//!   sequentially, so minting one extra entity earlier in the build silently
//!   reshuffles the psychology of every settlement downstream of it:
//!   deterministic, reproducible, and catastrophic.
//! - **`BakeId` (`Community.lineage`) — rejected, and worse.** `Bake::mint`
//!   is the same sequential counter in another costume, and inside the bake
//!   it is *circular*: disposition drives raiding drives founding and closing
//!   drives `BakeId` assignment. Task 4 makes warlikeness read this draw, so
//!   a `BakeId` key would close a feedback loop through the RNG.
//! - **The bare committed `cell-id` — necessary but not sufficient, and the
//!   reason is checked below rather than asserted.** The bake's `node_index`
//!   holds one *alive* community per cell, so a bare cell key separates the
//!   settlements standing at `now` — and nothing more. `Bake::vacant_habitable`
//!   excludes only cells an *alive* community holds, so a dead community's cell
//!   is re-settleable; and `Bake::relocate`'s conquest path opens the raider's
//!   record at the victim's cell in the very year the victim's record closes.
//!   Successive occupations of one site are different settlements, and a bare
//!   cell key would hand them one and the same mind.
//!   `the_draw_key_is_reachable_and_its_uniqueness_has_the_measured_shape`
//!   measures the real population — and also records the pair's own measured
//!   limit, which is that it is unique among ALIVE settlements but not over
//!   ruins. Read that test before Task 4 uses this draw inside the bake.
//!
//! The key this file pins is the occupation record's **`(site,
//! founded-year)`** pair — "this settlement, founded *here*, *then*". A
//! relocation never edits a record; it opens a *new* one (`Bake::open`, reached
//! from both of `relocate`'s branches), so a record's own `site` and `founded`
//! are immutable once opened.
//!
//! ## Why the two-sided agreement test matters as much as the id test
//!
//! Task 4's raid gate (`Bake::takes_the_initiative`) fires *during* the bake,
//! on `Bake` communities that are not yet ledger entities — so it cannot call
//! the ledger-reading wrapper. The derivation therefore ships as a
//! key-taking function that both sides call. If the two sides ever consume a
//! different stream or run different arithmetic, the disposition a world
//! *reports* differs from the one its history was *baked with*: silent,
//! deterministic, and precisely this campaign's own failure mode. The bake
//! side holds a raw `f64` founding year; the ledger side holds the same year
//! after `Ledger::commit` quantized it to 8 significant digits. Both reduce it
//! through the one shared `occupation_draw_key`, and the tests below pin that.

use hornvale_kernel::{CellId, ComponentStore, EntityId, Fact, KindId, Seed, Value, World};
use hornvale_species::{Dispersion, MindVector};
use std::collections::BTreeMap;

use hornvale_worldgen::disposition::{
    occupation_draw_key, people_disposition, settlement_disposition,
};

/// The default bake grid (`BakeConfig::default_millennia`): years 0..=2000 in
/// steps of 25, every one of them an exact integer in `f64`.
const BAKE_START_YEAR: f64 = 0.0;
const BAKE_END_YEAR: f64 = 2000.0;
const BAKE_EPOCH_YEARS: f64 = 25.0;

/// A minimal world carrying exactly one settlement-shaped entity, with
/// `filler` unrelated entities minted *before* it so its `EntityId` differs
/// between two otherwise identical worlds. The facts are committed through
/// `Ledger::commit`, so `occ-founded` is quantized exactly as a real build's
/// is.
fn synthetic_settlement(
    filler: usize,
    site: CellId,
    founded: f64,
    people: &str,
) -> (World, EntityId) {
    let mut world = World::new(Seed(42));
    hornvale_settlement::register_concepts(&mut world.registry)
        .expect("settlement concepts register cleanly");
    hornvale_history::register_concepts(&mut world.registry)
        .expect("history concepts register cleanly");
    for _ in 0..filler {
        world.ledger.mint_entity();
    }
    let id = world.ledger.mint_entity();
    for (predicate, object) in [
        (hornvale_history::IS_OCCUPATION, Value::Flag(true)),
        (
            hornvale_history::OCC_PEOPLE,
            Value::Text(people.to_string()),
        ),
        (hornvale_history::OCC_SITE, Value::Number(f64::from(site.0))),
        (hornvale_history::OCC_FOUNDED, Value::Number(founded)),
        (hornvale_settlement::IS_SETTLEMENT, Value::Flag(true)),
        (
            hornvale_settlement::CELL_ID,
            Value::Number(f64::from(site.0)),
        ),
    ] {
        world
            .ledger
            .commit(
                Fact {
                    subject: id,
                    predicate: predicate.to_string(),
                    object,
                    place: None,
                    day: Some(founded),
                    provenance: "tolerance-draw-test".to_string(),
                },
                &world.registry,
            )
            .expect("a well-formed fact commits");
    }
    (world, id)
}

/// The Salt's rule, enforced: a settlement's disposition must not change when
/// unrelated entities are minted before it. `Ledger::mint_entity` assigns
/// sequentially, so an `EntityId`-keyed draw would silently reshuffle every
/// settlement's psychology on any insertion — deterministic, reproducible,
/// and catastrophic (spec D3).
///
/// Note this is deliberately *stronger* than appending an unrelated entity to
/// an already-built world: appending cannot move an existing settlement's
/// id at all, so that check would pass even against an id-keyed draw. Here
/// the two worlds hold the same settlement at genuinely different ids.
#[test]
fn a_settlements_disposition_survives_an_earlier_entity_being_minted() {
    let (early, early_id) = synthetic_settlement(0, CellId(4242), 725.0, "human");
    let (late, late_id) = synthetic_settlement(37, CellId(4242), 725.0, "human");
    assert_ne!(
        early_id, late_id,
        "the fixture must actually place the settlement at two different ids, \
         or this test proves nothing"
    );

    let before = settlement_disposition(&early, early_id).expect("human carries a mind");
    let after = settlement_disposition(&late, late_id).expect("human carries a mind");

    assert_eq!(
        before, after,
        "the draw is keyed on entity identity, not on the settlement's own \
         (site, founded-year) — minting 37 unrelated entities first moved this \
         settlement's mind"
    );
}

/// The other half of the id-independence claim: the key is *live*. A draw
/// that ignored its key entirely would pass the test above trivially.
#[test]
fn the_draw_moves_with_both_halves_of_its_key() {
    let (base, base_id) = synthetic_settlement(0, CellId(4242), 725.0, "human");
    let (other_site, other_site_id) = synthetic_settlement(0, CellId(4243), 725.0, "human");
    let (other_year, other_year_id) = synthetic_settlement(0, CellId(4242), 750.0, "human");

    let base = settlement_disposition(&base, base_id).expect("human carries a mind");
    let by_site = settlement_disposition(&other_site, other_site_id).expect("human carries a mind");
    let by_year = settlement_disposition(&other_year, other_year_id).expect("human carries a mind");

    assert_ne!(
        base, by_site,
        "the same people founded in a different cell drew the same mind — the \
         site half of the key is not reaching the stream"
    );
    assert_ne!(
        base, by_year,
        "the same people founded in the same cell in a different year drew the \
         same mind — the founding-year half of the key is not reaching the \
         stream. This is the half that separates a RELOCATED community from \
         its predecessor."
    );
}

/// The bake side holds a raw `f64` founding year; the ledger side holds that
/// year after `Ledger::commit` quantized it to 8 significant digits. If the
/// two reduce it differently they derive different streams — silently. Pinned
/// across the whole default bake grid.
#[test]
fn the_year_key_survives_the_ledgers_quantization() {
    let mut year = BAKE_START_YEAR;
    while year <= BAKE_END_YEAR {
        let (world, id) = synthetic_settlement(0, CellId(1234), year, "human");
        let committed = match world.ledger.value_of(id, hornvale_history::OCC_FOUNDED) {
            Some(Value::Number(n)) => *n,
            other => panic!("occ-founded must commit as a number, got {other:?}"),
        };
        assert_eq!(
            occupation_draw_key(committed),
            occupation_draw_key(year),
            "founding year {year} reduced to a different key after the ledger \
             quantized it (committed as {committed})"
        );
        year += BAKE_EPOCH_YEARS;
    }
}

/// Ruling 2's headline: the disposition reached through the bake-side key
/// path and the disposition reached through the ledger-side wrapper are the
/// same value, for the same settlement.
#[test]
fn the_bake_side_key_path_and_the_ledger_side_wrapper_agree() {
    let psyche = hornvale_species::psyche_registry();
    let dispersion = hornvale_species::dispersion_registry();
    let mut year = BAKE_START_YEAR;
    while year <= BAKE_END_YEAR {
        for people in ["human", "goblin", "gnoll", "hobgoblin", "kobold", "bugbear"] {
            let site = CellId(4242 + (year as u32));
            let (world, id) = synthetic_settlement(3, site, year, people);
            let via_ledger =
                settlement_disposition(&world, id).expect("every settling people carries a mind");
            // The bake side never sees the ledger: it holds the raw f64 year
            // and the `KindId` label straight off the occupation record.
            let via_key = people_disposition(
                Seed(42),
                site,
                occupation_draw_key(year),
                people,
                &psyche,
                &dispersion,
            )
            .expect("every settling people carries a mind");
            assert_eq!(
                via_ledger, via_key,
                "bake-side and ledger-side disagree for {people} at site \
                 {site:?} founded {year}: the world would REPORT a disposition \
                 its history was not BAKED with"
            );
        }
        year += BAKE_EPOCH_YEARS;
    }
}

/// **The stream golden.** A new permanent stream label is a save-format
/// contract, and three parts of this one had nothing pinning them:
///
/// 1. **The per-dimension consumption order.** `people_disposition`'s doc
///    calls it "a frozen consumption order, like every other stream in this
///    workspace" — but reversing which of the three draws feeds which axis
///    changes every settlement's mind in the world while every *behavioural*
///    test still passes, because each test asserts a property (independence,
///    range, zero-dispersion identity) that a permutation preserves.
/// 2. **The leg-string format**, `format!("{}/{}", site.0, founded_year)`.
///    Any change to the separator, the order, or the rendering derives a
///    different seed for every settlement that has ever existed.
/// 3. **The field order of the `MindVector` struct literal**, which decides
///    draw-to-axis assignment because Rust evaluates struct-literal fields in
///    written order.
///
/// One `assert_eq!` against a hard-coded vector closes all three. Both witness
/// peoples are chosen with **three distinct authored axes** (human 0.5/0.6/0.75,
/// bugbear 0.8/0.4/0.3) so that a permutation cannot go unnoticed, and both keys
/// were checked to produce **no clamped component** — a value pinned at exactly
/// 0.0 or 1.0 would be insensitive to the offset that produced it and would
/// weaken the golden. `site != founded_year` so that transposing the two halves
/// of the leg is also caught.
///
/// If this test fails, the question is never "what are the new numbers" — it is
/// whether the derivation was *meant* to move. A deliberate change needs an
/// epoch suffix (`settlement/disposition/v2`), never a re-pin in place.
#[test]
fn the_draw_is_byte_pinned_for_a_known_key() {
    let psyche = hornvale_species::psyche_registry();
    let dispersion = hornvale_species::dispersion_registry();

    assert_eq!(
        people_disposition(Seed(42), CellId(1234), 725, "human", &psyche, &dispersion)
            .expect("human carries a mind"),
        MindVector {
            threat_response: 0.243_024_837_524_210_17,
            deliberation_latency: 0.034_637_047_603_831_57,
            time_horizon: 0.983_524_581_729_466_8,
        },
        "the settlement/disposition/v1 draw moved for human at (site 1234, \
         year 725). Either the consumption order, the leg format, or the \
         arithmetic changed — all three are save-format contracts, and a \
         deliberate change needs an epoch suffix, not a re-pin."
    );

    assert_eq!(
        people_disposition(Seed(42), CellId(1234), 725, "bugbear", &psyche, &dispersion)
            .expect("bugbear carries a mind"),
        MindVector {
            threat_response: 0.653_157_050_013_834_4,
            deliberation_latency: 0.076_935_455_773_618_04,
            time_horizon: 0.433_442_618_131_123_9,
        },
        "the settlement/disposition/v1 draw moved for bugbear at (site 1234, \
         year 725) — see the human assertion above"
    );
}

/// A people authored with zero dispersion is a point, not a distribution: the
/// draw must return its authored vector *exactly*. This is the value Task 5's
/// mutation proof needs to mean what it says, and it is also the property
/// that makes `Dispersion { .. 0.0 }` describe the model's pre-Tolerance
/// behaviour.
#[test]
fn a_zero_dispersion_people_draws_its_authored_vector_exactly() {
    let mean = MindVector {
        threat_response: 0.85,
        deliberation_latency: 0.4,
        time_horizon: 0.3,
    };
    let psyche: ComponentStore<KindId, MindVector> =
        [(KindId("test-kind"), mean)].into_iter().collect();
    let dispersion: ComponentStore<KindId, Dispersion> = [(
        KindId("test-kind"),
        Dispersion {
            mind: 0.0,
            society: 0.0,
            perception: 0.0,
        },
    )]
    .into_iter()
    .collect();

    for year in [0, 725, 2000] {
        for cell in [0u32, 1, 4242] {
            let drawn = people_disposition(
                Seed(42),
                CellId(cell),
                year,
                "test-kind",
                &psyche,
                &dispersion,
            )
            .expect("the fixture kind carries a mind");
            assert_eq!(
                drawn, mean,
                "zero dispersion must be the identity, but cell {cell} year \
                 {year} moved off the authored vector"
            );
        }
    }
}

/// The bounded-axis clamp holds on every real people, every dimension. This
/// is the clamp whose *bias* the derivation's doc comment discloses — the
/// clamp itself must never leak an out-of-range ratio.
#[test]
fn every_drawn_dimension_stays_inside_the_unit_interval() {
    let psyche = hornvale_species::psyche_registry();
    let dispersion = hornvale_species::dispersion_registry();
    for (kind, _) in psyche.iter() {
        for cell in 0u32..400 {
            let drawn = people_disposition(
                Seed(42),
                CellId(cell),
                i64::from(cell) * 5,
                kind.0,
                &psyche,
                &dispersion,
            )
            .expect("every kind in psyche_registry carries a mind");
            for (axis, v) in [
                ("threat_response", drawn.threat_response),
                ("deliberation_latency", drawn.deliberation_latency),
                ("time_horizon", drawn.time_horizon),
            ] {
                assert!(
                    (0.0..=1.0).contains(&v),
                    "{} drew {axis} = {v} at cell {cell}, outside [0, 1]",
                    kind.0
                );
            }
        }
    }
}

/// The three dimensions are drawn independently (task ruling 4). One draw
/// shared across all three would make them perfectly correlated — a strong
/// claim that "a spread around a point in 3-space" does not imply. The
/// witness: a kind whose three authored dimensions are equal must still draw
/// three *different* values.
#[test]
fn the_three_mind_dimensions_are_drawn_independently() {
    // goblin sits on the manikin: 0.5 on all three axes. If the draw shared
    // one offset across dimensions, its drawn vector would stay on the
    // diagonal for every settlement.
    let psyche = hornvale_species::psyche_registry();
    let dispersion = hornvale_species::dispersion_registry();
    let goblin = psyche.get_by_label("goblin").expect("goblin has a mind");
    assert_eq!(
        (goblin.threat_response, goblin.deliberation_latency),
        (goblin.time_horizon, goblin.time_horizon),
        "this test's witness requires goblin's three axes to be equal; \
         re-pick the witness kind if the roster changed"
    );
    let mut off_diagonal = 0;
    for cell in 0u32..64 {
        let drawn = people_disposition(Seed(42), CellId(cell), 100, "goblin", &psyche, &dispersion)
            .expect("goblin has a mind");
        if drawn.threat_response != drawn.deliberation_latency
            || drawn.deliberation_latency != drawn.time_horizon
        {
            off_diagonal += 1;
        }
    }
    assert_eq!(
        off_diagonal, 64,
        "goblin's drawn vectors stayed on the diagonal: the three dimensions \
         share one draw and are perfectly correlated"
    );
}

/// A kind with no authored mind has no disposition to draw — the wrapper says
/// so rather than inventing the manikin.
#[test]
fn a_people_with_no_authored_mind_has_no_disposition() {
    let (world, id) = synthetic_settlement(0, CellId(7), 100.0, "not-a-people");
    assert_eq!(settlement_disposition(&world, id), None);
}

/// An entity that is not an occupation carries neither half of the key.
#[test]
fn an_entity_that_is_not_an_occupation_has_no_disposition() {
    let (mut world, _) = synthetic_settlement(0, CellId(7), 100.0, "human");
    let bare = world.ledger.mint_entity();
    assert_eq!(settlement_disposition(&world, bare), None);
}

/// What the draw key actually is on real worlds — reachability, and the exact
/// shape of its uniqueness. **This test reports a measured limit, not a clean
/// bill of health.** Read it before Task 4 uses this draw inside the bake.
///
/// Three findings, all measured here over three seeds and replicated out of
/// band over seeds 1..=12:
///
/// 1. **The key is reachable on both sides.** Every occupation record commits
///    `occ-site` and `occ-founded`; every *alive* one additionally commits the
///    settlement-side `cell-id`, and the two always name the same cell.
/// 2. **Among alive settlements the key is unique** — and that is exactly the
///    population [`settlement_disposition`] is defined on, because the wrapper
///    requires `cell-id` and a ruin never commits one. The ledger-side contract
///    is therefore sound.
/// 3. **Over ALL occupation records the key is NOT unique**, at roughly 3–15%
///    of records depending on seed (92 of 862 at seed 1; 130 of 919 at seed
///    42). The collisions are not arbitrary, and their shape is asserted below
///    because it is what makes finding 2 true: *every* colliding group holds at
///    most one alive record and at least one **zero-tenure** record — a
///    community that opened and closed inside a single epoch, which is what
///    `Bake::relocate`'s conquest path produces when it opens the raider's
///    record at the victim's cell in the same year the victim's record closes.
///
/// The open question this hands to Task 4: a zero-tenure transient and the
/// record that displaced it share a key, so they would share a drawn mind. No
/// two *simultaneously alive* communities ever do (`Bake.node_index` holds one
/// alive community per cell), so the raid gate cannot see two live communities
/// with one disposition — but it can see a transient inherit its successor's.
/// Whether that matters is a question about the raid gate, and it is
/// deliberately left to the task that owns it rather than resolved here by
/// improvising a third key component.
///
/// Stays in the COMMIT GATE rather than the heavy tier: three
/// `BuildDepth::Settlements` builds measure ~5 s together, and this is the
/// guard that would catch the one failure this task cannot recover from — a
/// draw key that is not actually reachable, or whose uniqueness has quietly
/// changed shape. That is worth five seconds every commit.
#[test]
fn the_draw_key_is_reachable_and_its_uniqueness_has_the_measured_shape() {
    use hornvale_astronomy::SkyPins;
    use hornvale_terrain::TerrainPins;
    use hornvale_worldgen::{
        BuildDepth, SettlementPins, SkyChoice, WorldComponents, build_world_to,
    };
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    for seed in [1u64, 42, 777] {
        let world = build_world_to(
            Seed(seed),
            &SkyPins::default(),
            SkyChoice::Generated,
            &TerrainPins::default(),
            &SettlementPins::default(),
            &wc,
            BuildDepth::Settlements,
        )
        .expect("an unpinned world builds");

        let ids: Vec<EntityId> = world
            .ledger
            .find(hornvale_history::IS_OCCUPATION)
            .map(|f| f.subject)
            .collect();
        let alive_count = world
            .ledger
            .find(hornvale_settlement::IS_SETTLEMENT)
            .count();
        assert!(
            alive_count > 0 && alive_count < ids.len(),
            "seed {seed}: {alive_count} alive of {} occupation records — this test \
             needs BOTH standing settlements and ruins to measure what it claims",
            ids.len()
        );

        // (site, founded-year, is-alive, is-zero-tenure) per record.
        let mut rows: Vec<((u32, i64), bool, bool)> = Vec::new();
        for id in ids {
            let site = match world.ledger.value_of(id, hornvale_history::OCC_SITE) {
                Some(Value::Number(n)) => *n as u32,
                other => {
                    panic!("seed {seed}: occupation {id:?} has no numeric occ-site ({other:?})")
                }
            };
            let founded = match world.ledger.value_of(id, hornvale_history::OCC_FOUNDED) {
                Some(Value::Number(n)) => *n,
                other => panic!(
                    "seed {seed}: occupation {id:?} has no numeric occ-founded ({other:?}) — \
                     the (site, founded-year) key is NOT reachable on the ledger side"
                ),
            };
            let ended = match world.ledger.value_of(id, hornvale_history::OCC_ENDED) {
                Some(Value::Number(n)) => Some(*n),
                _ => None,
            };
            // FINDING 1: where both the occupation-side site and the
            // settlement-side cell exist they must name the same cell — the
            // wrapper reads `cell-id`, the bake holds `site`, and they are one
            // key only if they agree.
            let alive = match world.ledger.value_of(id, hornvale_settlement::CELL_ID) {
                Some(Value::Number(n)) => {
                    assert_eq!(
                        site, *n as u32,
                        "seed {seed}: occupation {id:?} committed occ-site {site} but cell-id {n}"
                    );
                    assert!(
                        settlement_disposition(&world, id).is_some(),
                        "seed {seed}: settlement {id:?} has a reachable key but no disposition"
                    );
                    true
                }
                _ => {
                    // A ruin commits no `cell-id`, so the wrapper is undefined
                    // on it — which is precisely why finding 2 is enough.
                    assert_eq!(
                        settlement_disposition(&world, id),
                        None,
                        "seed {seed}: occupation {id:?} has no cell-id but yielded a disposition"
                    );
                    false
                }
            };
            rows.push((
                (site, occupation_draw_key(founded)),
                alive,
                ended == Some(founded),
            ));
        }

        // FINDING 2: unique among the alive settlements — the wrapper's domain.
        let mut alive_keys: Vec<(u32, i64)> = rows.iter().filter(|r| r.1).map(|r| r.0).collect();
        let alive_total = alive_keys.len();
        alive_keys.sort_unstable();
        alive_keys.dedup();
        assert_eq!(
            alive_keys.len(),
            alive_total,
            "seed {seed}: two ALIVE settlements share a (site, founded-year) draw key — \
             the ledger-side contract is broken and two standing settlements would \
             report one and the same mind"
        );

        // FINDING 3: not unique over all records — and every colliding group
        // holds at most one alive record and at least one zero-tenure one.
        let mut groups: BTreeMap<(u32, i64), Vec<(bool, bool)>> = BTreeMap::new();
        for (key, alive, zero_tenure) in &rows {
            groups.entry(*key).or_default().push((*alive, *zero_tenure));
        }
        let colliding: Vec<_> = groups.values().filter(|g| g.len() > 1).collect();
        assert!(
            !colliding.is_empty(),
            "seed {seed}: no (site, founded-year) collisions at all. That is BETTER \
             than the measured behaviour, not worse — but this test is documenting \
             a known limit, so re-measure and rewrite it rather than deleting it."
        );
        for group in &colliding {
            assert!(
                group.iter().filter(|(alive, _)| *alive).count() <= 1,
                "seed {seed}: a (site, founded-year) group holds more than one ALIVE \
                 record — the property finding 2 rests on has changed shape"
            );
            assert!(
                group.iter().any(|(_, zero_tenure)| *zero_tenure),
                "seed {seed}: a (site, founded-year) collision with NO zero-tenure \
                 member — collisions are no longer confined to the within-epoch \
                 conquest transient, and Task 4's exposure is wider than documented"
            );
        }
        let extra: usize = colliding.iter().map(|g| g.len() - 1).sum();
        println!(
            "seed {seed}: {} occupation records ({alive_count} alive), {} distinct \
             (site, founded-year) keys, {} colliding keys, {extra} records sharing \
             a key with an earlier one — all of them within-epoch transients",
            rows.len(),
            groups.len(),
            colliding.len()
        );
    }
}
