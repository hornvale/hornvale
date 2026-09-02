//! The resident draws (The Roll, spec §3.1–§3.6): what a settlement's
//! residents DRAW — a name, an age, a deviation on three mind dials — on one
//! fresh stream per resident, keyed on the settlement's site vertex and the
//! resident's ordinal. Nothing here mints an entity; `windows/vessel`'s
//! `derive_residents` does that and reads these.
//!
//! Same mechanism as [`crate::disposition`], one rung finer: that module
//! perturbs a PEOPLE's mean into a SETTLEMENT's disposition, keyed on the
//! occupation's (site, founded-year); this one perturbs the species mean
//! into a RESIDENT's, keyed on (site, ordinal). Keyed on the site and never
//! on an `EntityId` for the reason that module's doc gives.
#![warn(missing_docs)]

use std::collections::BTreeSet;

use hornvale_kernel::seed::StreamLabel;
use hornvale_kernel::{KindId, Seed, Stream, Value, World};
use hornvale_language::{NameKind, Namer};
use hornvale_settlement::{VERTEX_ID, VillageInfo};
use hornvale_species::{Dispersion, MindVector};

use crate::components::WorldComponents;
use crate::disposition::perturb;

/// One resident's drawn traits.
/// type-audit: bare-ok(index: ordinal), bare-ok(identifier-text: name), bare-ok(count: age_days)
#[derive(Clone, Debug, PartialEq)]
pub struct ResidentDraw {
    /// Which resident of its settlement this is (the lineage ordinal).
    pub ordinal: u16,
    /// Its name, from its people's own namer; distinct within the settlement.
    pub name: String,
    /// Its age at derivation, in standard days, uniform on `[0, lifespan)` —
    /// a population of mixed age, not a cohort.
    pub age_days: f64,
    /// The species mean perturbed by the kind's `Dispersion.mind`, clamped.
    pub mind: MindVector,
}

/// The one place a resident's key becomes a stream.
fn resident_stream(seed: Seed, site: u32, ordinal: u16) -> Stream {
    let leg = format!("{site}/{ordinal}");
    seed.derive(crate::streams::SETTLEMENT_RESIDENT)
        .derive(StreamLabel::dynamic(&leg))
        .stream()
}

/// The residents of `village`, `village.population` of them, ordinal ==
/// index. `species` is the settlement's `peopled-by` label (read with
/// `hornvale_species::species_of`). TOTAL over the KIND lookups, never
/// absent: a kind with no language row draws no name (falls to
/// `"{species} {ordinal+1}"`), no psyche row draws the manikin mind, and no
/// biosphere row draws age zero — the same neutral posture `body_at`'s
/// lookups take on a miss, so a `Body.species` typo reads as a neutral
/// resident rather than a missing one. The SITE is not part of that
/// fallback (see `# Panics`): every settlement is minted at genesis with a
/// `cell-id` fact (lexicon: frozen predicate VALUE, decision 0246), so its
/// absence (`domains/settlement/src/genesis.rs`) is a corrupted world, not
/// a degraded-but-total read.
///
/// # Panics
///
/// Panics if `village.id` carries no `VERTEX_ID` (the frozen predicate
/// `cell-id`, lexicon: decision 0246) fact, or a non-numeric one. A
/// resident's stream is keyed on its settlement's site; silently
/// defaulting to vertex 0 would alias that settlement's residents onto
/// vertex 0's own stream — a real vertex, not a sentinel — which is
/// exactly the (site, ordinal) contract's aliasing hazard this fails
/// loudly against instead.
/// type-audit: bare-ok(identifier-text: species)
pub fn resident_draws(
    world: &World,
    wc: &WorldComponents,
    village: &VillageInfo,
    species: &str,
) -> Vec<ResidentDraw> {
    let site = match world.ledger.value_of(village.id, VERTEX_ID) {
        Some(Value::Number(n)) => *n as u32,
        _ => panic!(
            "settlement {:?} ({}) carries no cell-id fact; a resident's stream is keyed on its site and cannot be derived without one", // lexicon: frozen predicate VALUE, decision 0246
            village.id, village.name
        ),
    };
    let kind: Option<&'static str> = wc
        .biosphere
        .iter()
        .find(|(k, _)| k.0 == species)
        .map(|(k, _)| k.0);
    let mean = hornvale_species::psyche_registry()
        .get_by_label(species)
        .copied()
        .unwrap_or(MindVector::MANIKIN);
    let spread = hornvale_species::dispersion_registry()
        .get_by_label(species)
        .copied()
        .unwrap_or(Dispersion {
            mind: 0.0,
            society: 0.0,
            perception: 0.0,
        });
    let lifespan_days = wc
        .biosphere
        .get_by_label(species)
        .and_then(|b| {
            hornvale_species::life_history(b.mass, b.thermal_strategy, b.schedule).lifespan
        })
        .map_or(0.0, |y| y.days());
    // One namer per settlement (a phonology draw is not free); `None` for a
    // kind that cannot be named, which falls to the ordinal form.
    let namer = kind.and_then(|k| {
        let mind = wc.psyche.get(&KindId(k))?;
        let society = wc.society.get(&KindId(k))?;
        wc.articulation.get(&KindId(k))?;
        let ph = crate::language_of_wc(world, wc, k);
        Some((ph, crate::morph_options(mind, society), k))
    });
    let mut taken: BTreeSet<String> = BTreeSet::new();
    let count = village.population.min(u32::from(u16::MAX)) as u16;
    let mut out = Vec::with_capacity(usize::from(count));
    for ordinal in 0..count {
        let mut stream = resident_stream(world.seed, site, ordinal);
        // ORDER IS A CONTRACT (`the_stream_order_is_pinned`): salt, age, dials.
        let salt = stream.next_u64();
        let age_days = stream.next_f64() * lifespan_days;
        let mind = MindVector {
            threat_response: perturb(&mut stream, mean.threat_response, spread.mind),
            deliberation_latency: perturb(&mut stream, mean.deliberation_latency, spread.mind),
            time_horizon: perturb(&mut stream, mean.time_horizon, spread.mind),
        };
        let drawn = namer.as_ref().map(|(ph, morph, k)| {
            Namer::new(&world.seed, k, ph)
                .name(NameKind::Person, salt, morph)
                .roman
        });
        let mut name = drawn.unwrap_or_else(|| format!("{species} {}", ordinal + 1));
        if !taken.insert(name.clone()) {
            name = format!("{name} {}", ordinal + 1);
            taken.insert(name.clone());
        }
        out.push(ResidentDraw {
            ordinal,
            name,
            age_days,
            mind,
        });
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    // Named construction site (decision 0092): builds its own world once per
    // test — the sanctioned test-fixture posture the weir's spec carves out
    // (`windows/worldgen/tests/suite/exposure.rs` uses the identical shape).
    #[allow(clippy::disallowed_methods)]
    fn world() -> World {
        crate::build_world(
            hornvale_kernel::Seed(42),
            &hornvale_astronomy::SkyPins::default(),
            crate::SkyChoice::Generated,
            &hornvale_terrain::TerrainPins::default(),
            &crate::SettlementPins::default(),
        )
        .unwrap()
    }

    fn fixture() -> (World, WorldComponents, VillageInfo, String) {
        let w = world();
        let village = hornvale_settlement::village_info(&w).expect("seed 42 places a flagship");
        let species =
            hornvale_species::species_of(&w, village.id).expect("the flagship is peopled");
        let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
        (w, wc, village, species)
    }

    /// The draw is a pure function of (world, settlement, ordinal): the same
    /// call twice yields byte-equal draws.
    ///
    /// MUTATION THIS MUST FAIL AGAINST: in `resident_stream`, append
    /// `village.population` to the leg — this test stays green, and
    /// `a_draw_does_not_depend_on_the_cohort` reddens; both exist for that
    /// reason. The mutation that reddens THIS one is seeding the stream from
    /// `Seed(0)` instead of `world.seed` in one of the two calls, which is a
    /// test-harness mutation and is stated so a reader knows the property
    /// is determinism, not independence.
    #[test]
    fn draws_are_deterministic() {
        let (w, wc, village, species) = fixture();
        let a = resident_draws(&w, &wc, &village, &species);
        let b = resident_draws(&w, &wc, &village, &species);
        assert_eq!(a, b);
    }

    /// Resident `i`'s draw does not depend on how many residents the
    /// settlement has: a `VillageInfo` with `population: 3` yields the same
    /// first three draws as one with `population: 6`. Coarse constrains fine:
    /// a resident is keyed by its own ordinal, not by its cohort.
    ///
    /// MUTATION THIS MUST FAIL AGAINST: fold `village.population` into the
    /// stream leg in `resident_stream`; draw 0 then differs between the two.
    #[test]
    fn a_draw_does_not_depend_on_the_cohort() {
        let (w, wc, village, species) = fixture();
        let mut small = village.clone();
        small.population = 3;
        let mut large = village.clone();
        large.population = 6;
        let small_draws = resident_draws(&w, &wc, &small, &species);
        let large_draws = resident_draws(&w, &wc, &large, &species);
        assert_eq!(small_draws.len(), 3);
        assert_eq!(large_draws.len(), 6);
        assert_eq!(small_draws[..3], large_draws[..3]);
    }

    /// Names are distinct within a settlement (spec §3.5): a collision is
    /// resolved by suffixing the ordinal, never by dropping a resident.
    ///
    /// MUTATION THIS MUST FAIL AGAINST: replace `salt` with the literal `0`
    /// in the `name(..)` call and delete the collision branch; every name
    /// collides and the set is smaller than the vector.
    #[test]
    fn names_are_distinct_within_a_settlement() {
        let (w, wc, village, species) = fixture();
        let draws = resident_draws(&w, &wc, &village, &species);
        let names: BTreeSet<&str> = draws.iter().map(|d| d.name.as_str()).collect();
        assert_eq!(names.len(), draws.len());
    }

    /// Every dial is a ratio after the draw, and in a settlement of >= 8 at
    /// least one resident differs from the species mean on at least one dial
    /// (`dispersion_is_a_ratio_on_every_axis` pins the spread rows non-zero
    /// for every minded kind).
    ///
    /// MUTATION THIS MUST FAIL AGAINST: pass `0.0` as `spread` to the three
    /// `perturb` calls; every dial equals the mean and this reddens.
    #[test]
    fn dials_are_clamped_ratios_and_vary() {
        let (w, wc, mut village, species) = fixture();
        village.population = village.population.max(8);
        let draws = resident_draws(&w, &wc, &village, &species);
        let mean = hornvale_species::psyche_registry()
            .get_by_label(&species)
            .copied()
            .unwrap_or(MindVector::MANIKIN);
        for d in &draws {
            assert!((0.0..=1.0).contains(&d.mind.threat_response));
            assert!((0.0..=1.0).contains(&d.mind.deliberation_latency));
            assert!((0.0..=1.0).contains(&d.mind.time_horizon));
        }
        assert!(
            draws.iter().any(|d| {
                d.mind.threat_response != mean.threat_response
                    || d.mind.deliberation_latency != mean.deliberation_latency
                    || d.mind.time_horizon != mean.time_horizon
            }),
            "no resident differed from the species mean on any dial"
        );
    }

    /// The consumption order inside a resident's stream is a save-format
    /// contract: name salt, age, then the three dials in `MindVector`
    /// declaration order. Pinned BY VALUE at seed 42, the flagship
    /// settlement (peopled by `bugbear`), resident ordinal 1 — the literals
    /// below are what `resident_draws` produced there on first green.
    ///
    /// MUTATION THIS MUST FAIL AGAINST: move the `age_days` draw below the
    /// three dials; every pinned value moves.
    #[test]
    fn the_stream_order_is_pinned() {
        let (w, wc, village, species) = fixture();
        let draws = resident_draws(&w, &wc, &village, &species);
        let d = &draws[1];
        assert_eq!(d.name, "Dvoashngashngo");
        assert_eq!(d.age_days, 16993.545284545915);
        assert_eq!(d.mind.threat_response, 0.6016664724049925);
        assert_eq!(d.mind.deliberation_latency, 0.5346607350443859);
        assert_eq!(d.mind.time_horizon, 0.5453806270496363);
    }

    /// A settlement with no `cell-id` fact (lexicon: frozen predicate
    /// VALUE, decision 0246) is a corrupted world (every settlement is
    /// minted with one at genesis, `domains/settlement/src/genesis.rs`),
    /// and `resident_draws` fails loudly rather than silently aliasing its
    /// residents onto vertex 0's own stream.
    ///
    /// MUTATION THIS MUST FAIL AGAINST: restore the `_ => 0` arm in
    /// `resident_draws`' site match; this test then reddens because nothing
    /// panics (an aliased vertex-0 read instead).
    #[test]
    #[should_panic(expected = "carries no cell-id")] // lexicon: frozen predicate VALUE, decision 0246
    fn a_settlement_with_no_site_fails_loudly() {
        let (w, wc, _village, species) = fixture();
        let mut ghost_world = w.clone();
        let ghost_id = ghost_world
            .ledger
            .mint_entity(hornvale_kernel::test_lineage(u16::MAX));
        let ghost_village = VillageInfo {
            id: ghost_id,
            name: "Ghost Camp".to_string(),
            population: 1,
        };
        resident_draws(&ghost_world, &wc, &ghost_village, &species);
    }
}
