use hornvale_history::record::CauseOfEnd;
use hornvale_kernel::{KindId, Vertex};
use hornvale_lot::context::assemble;
use hornvale_lot::draw::{CauseProvenance, DeathCause, Ending, draw, odds_at};
use hornvale_lot::endemic::{HazardBand, cause_weights_for_band, endemic_burden_at};
use hornvale_lot::json::{life_json, odds_json};
use hornvale_lot::narrate::narrate;
use hornvale_lot::projection::{Projection, ProjectionMateriality, SourceCohort};
use hornvale_lot::slots::{SlotValue, Source, tell};
use hornvale_lot::{LotIndex, Pick};
use hornvale_worldgen::emit_history;

#[test]
fn endemic_flux_weight_reads_the_authoritative_population_substrate() {
    let world = hornvale_worldgen::seed_42_world();
    let ctx = assemble(&world).unwrap();
    let year = ctx.present_year - 1.0;
    let site = ctx
        .occupations
        .iter()
        .find(|prepared| prepared.record.core.is_alive())
        .expect("seed 42 has a living occupation")
        .record
        .core
        .site;
    let substrate = hornvale_worldgen::bake_era_population_view(&world).unwrap();
    let era = substrate
        .rows()
        .filter(|row| row.era_start <= year)
        .map(|row| row.era_start)
        .fold(None, |_, era| Some(era))
        .expect("the picked year has an era");
    let population = substrate.population_at(era, site);
    let expected = 0.35 * (0.5 + 0.5 * (population / 50.0).min(1.0));

    let burden = endemic_burden_at(&ctx, site, year);
    let flux = burden
        .iter()
        .find(|(kind, _)| *kind == KindId("the-flux"))
        .expect("the flux is present everywhere");
    assert_eq!(flux.1.background_weight.to_bits(), expected.to_bits());
    assert_eq!(flux.1.infant_weight, 0.55);
}

#[test]
fn every_band_has_a_normalized_cause_distribution() {
    let world = hornvale_worldgen::seed_42_world();
    let ctx = assemble(&world).unwrap();
    for &site in ctx.settlements_by_vertex.keys() {
        for year in [ctx.start_year, ctx.present_year - 1.0] {
            for strife in [0.0, 0.25, 0.5, 1.0] {
                for band in [
                    HazardBand::Infant,
                    HazardBand::Background,
                    HazardBand::Senescent,
                ] {
                    let weights = cause_weights_for_band(&ctx, site, year, strife, band);
                    let sum: f64 = weights.iter().map(|weight| weight.weight).sum();
                    assert!(
                        (sum - 1.0).abs() < 1e-12,
                        "site {site:?}, year {year}, strife {strife}, {band:?} sums to {sum}"
                    );
                }
            }
        }
    }
}

#[test]
fn outbreak_probability_is_spliced_into_the_life_course() {
    assert!(!hornvale_lot::draw::outbreak_kills(20.0, 100.0, 0.20));
    assert!(hornvale_lot::draw::outbreak_kills(20.0, 100.0, 0.199_999));
}

#[test]
fn real_seed_produces_named_outbreak_and_plague_endings() {
    let world = hornvale_worldgen::seed_42_world();
    let ctx = assemble(&world).unwrap();
    let mut outbreak = None;
    let mut plague = None;
    for index in 0..200_000 {
        let life = draw(&ctx, LotIndex(index), &Pick::default()).unwrap();
        match (&life.ending, &life.cause) {
            (Ending::Outbreak(kind), Some(DeathCause::Pathogen(cause))) => {
                assert_eq!(kind, cause);
                outbreak.get_or_insert(index);
            }
            (Ending::CommunityFate(CauseOfEnd::Plague), Some(DeathCause::Pathogen(kind))) => {
                assert!(matches!(kind.0, "the-pest" | "the-pox"));
                plague.get_or_insert(index);
            }
            _ => {}
        }
        if outbreak.is_some() && plague.is_some() {
            break;
        }
    }
    assert!(
        outbreak.is_some(),
        "no outbreak death in 200,000 deterministic lots"
    );
    assert!(
        plague.is_some(),
        "no named Plague death in 200,000 deterministic lots"
    );
}

#[test]
fn plague_rendering_cites_the_closing_event_and_paired_facts() {
    let world = hornvale_worldgen::seed_42_world();
    let ctx = assemble(&world).unwrap();
    let life = (0..200_000)
        .filter_map(|index| draw(&ctx, LotIndex(index), &Pick::default()).ok())
        .find(|life| {
            matches!(
                (&life.ending, &life.cause, &life.cause_provenance),
                (
                    Ending::CommunityFate(CauseOfEnd::Plague),
                    Some(DeathCause::Pathogen(_)),
                    Some(CauseProvenance::Outbreak { .. })
                )
            )
        })
        .expect("seed 42 yields a named Plague death");
    let story = tell(&world, &ctx, &life);
    let cause = story.slot("cause").expect("cause slot exists");
    let (event, pathogen) = match life.cause_provenance {
        Some(CauseProvenance::Outbreak {
            event, pathogen, ..
        }) => (event, pathogen),
        other => panic!("Plague death has unexpected provenance: {other:?}"),
    };

    assert_eq!(cause.value, SlotValue::Filled(pathogen.0.replace('-', " ")));
    let fact_sources: Vec<_> = cause
        .sources
        .iter()
        .filter_map(|source| match source {
            Source::Fact {
                entity, predicate, ..
            } if *entity == event.get()
                && (*predicate == hornvale_epidemiology::STRUCK_BY
                    || *predicate == hornvale_epidemiology::OUTBREAK_DEATHS) =>
            {
                Some(predicate.clone())
            }
            _ => None,
        })
        .collect();
    assert_eq!(
        fact_sources,
        vec![
            hornvale_epidemiology::STRUCK_BY,
            hornvale_epidemiology::OUTBREAK_DEATHS
        ]
    );
    assert_eq!(
        cause
            .sources
            .iter()
            .filter(|source| matches!(
                source,
                Source::Derived { function, .. } if *function == "lot::draw::hazard_cause"
            ))
            .count(),
        0,
        "Outbreak/Plague provenance must not cite hazard_cause"
    );

    let struck = world
        .ledger
        .facts_of(event, hornvale_epidemiology::STRUCK_BY)
        .next()
        .expect("closing event has struck-by fact");
    let deaths = world
        .ledger
        .facts_of(event, hornvale_epidemiology::OUTBREAK_DEATHS)
        .next()
        .expect("closing event has outbreak-deaths fact");
    assert_eq!(struck.subject, deaths.subject);
    assert_eq!(struck.place, deaths.place);
    assert_eq!(struck.day, deaths.day);
}

#[test]
fn emitted_interleaved_rehit_keeps_the_closing_a_event_through_lot_rendering() {
    let components = hornvale_worldgen::WorldComponents::assemble().unwrap();
    let mut world = hornvale_worldgen::build_world_to(
        hornvale_kernel::Seed(42),
        &hornvale_astronomy::SkyPins::default(),
        &hornvale_terrain::TerrainPins::default(),
        &hornvale_worldgen::SettlementPins::default(),
        &components,
        hornvale_worldgen::BuildDepth::Terrain,
    )
    .unwrap();
    let site = Vertex(7);
    // The real bake applies A (nonlethal), B (nonlethal), then A (closing).
    // Its aggregate moves the closing A to the emitted position, preserving
    // A's event identity through the History crossing.
    let history = hornvale_worldgen::interleaved_rehit_history(site);
    emit_history(&mut world, &history).unwrap();
    let world_entity = world
        .ledger
        .find("sky-provider")
        .next()
        .expect("terrain world has a scenario pin")
        .subject;
    hornvale_worldgen::emit_now(&mut world, world_entity, history.now).unwrap();

    let ctx = assemble(&world).unwrap();
    let pick = Pick {
        year: Some(1.0),
        site: Some(site),
    };
    let life = (0..200_000)
        .filter_map(|index| draw(&ctx, LotIndex(index), &pick).ok())
        .find(|life| {
            matches!(
                (&life.ending, &life.cause, &life.cause_provenance),
                (
                    Ending::CommunityFate(CauseOfEnd::Plague),
                    Some(DeathCause::Pathogen(KindId("the-pest"))),
                    Some(CauseProvenance::Outbreak {
                        pathogen: KindId("the-pest"),
                        ..
                    })
                )
            )
        })
        .expect("the emitted A/B/A fixture yields a closing A Plague life");
    let story = tell(&world, &ctx, &life);
    let cause = story.slot("cause").expect("cause slot exists");
    let event = match life.cause_provenance {
        Some(CauseProvenance::Outbreak { event, .. }) => event,
        other => panic!("fixture has unexpected provenance: {other:?}"),
    };
    assert_eq!(cause.value, SlotValue::Filled("the pest".to_string()));
    assert_eq!(
        cause
            .sources
            .iter()
            .filter(|source| matches!(
                source,
                Source::Derived { function, .. } if *function == "lot::draw::hazard_cause"
            ))
            .count(),
        0,
        "Outbreak/Plague provenance must not cite hazard_cause"
    );
    assert!(cause.sources.iter().any(|source| matches!(
        source,
        Source::Fact { entity, predicate, .. }
            if *entity == event.get() && predicate == hornvale_epidemiology::STRUCK_BY
    )));
    assert!(cause.sources.iter().any(|source| matches!(
        source,
        Source::Fact { entity, predicate, .. }
            if *entity == event.get() && predicate == hornvale_epidemiology::OUTBREAK_DEATHS
    )));
    let struck = world
        .ledger
        .facts_of(event, hornvale_epidemiology::STRUCK_BY)
        .next()
        .expect("closing A has struck-by fact");
    let deaths = world
        .ledger
        .facts_of(event, hornvale_epidemiology::OUTBREAK_DEATHS)
        .next()
        .expect("closing A has outbreak-deaths fact");
    assert_eq!(struck.subject, event);
    assert_eq!(deaths.subject, event);
    assert_eq!(struck.place, Some(life.ending_occupation));
    assert_eq!(deaths.place, Some(life.ending_occupation));
    assert_eq!(struck.day, deaths.day);
}

#[test]
fn every_dead_lot_has_a_sourced_cause_slot() {
    let world = hornvale_worldgen::seed_42_world();
    let ctx = assemble(&world).unwrap();
    let mut dead = 0;
    for index in 0..200 {
        let life = draw(&ctx, LotIndex(index), &Pick::default()).unwrap();
        if life.ending == Ending::Alive {
            continue;
        }
        let story = tell(&world, &ctx, &life);
        let cause = story.slot("cause").expect("the cause slot is always asked");
        assert!(matches!(cause.value, SlotValue::Filled(_)));
        assert!(!cause.sources.is_empty());
        let hazard_sources = cause
            .sources
            .iter()
            .filter(|source| {
                matches!(
                    source,
                    Source::Derived { function, .. } if *function == "lot::draw::hazard_cause"
                )
            })
            .count();
        assert_eq!(
            hazard_sources,
            if matches!(life.cause_provenance, Some(CauseProvenance::Hazard { .. })) {
                1
            } else {
                0
            },
            "hazard_cause is exclusive to continuous hazard deaths"
        );
        match life.cause_provenance.as_ref() {
            Some(CauseProvenance::Outbreak { event, .. }) => {
                assert!(cause.sources.iter().any(|source| matches!(
                    source,
                    Source::Fact { entity, predicate, .. }
                        if *entity == event.get()
                            && (predicate == hornvale_epidemiology::STRUCK_BY
                                || predicate == hornvale_epidemiology::OUTBREAK_DEATHS)
                )))
            }
            Some(CauseProvenance::CommunityFate { occupation, .. }) => {
                assert!(cause.sources.iter().any(|source| matches!(
                    source,
                    Source::Fact { entity, predicate, .. }
                        if *entity == occupation.get()
                            && predicate == hornvale_history::OCC_CAUSE
                )))
            }
            Some(CauseProvenance::Hazard { .. }) => {
                assert!(cause.sources.iter().any(|source| matches!(
                    source,
                    Source::Derived { function, .. } if *function == "lot::draw::hazard_cause"
                )))
            }
            None => panic!("dead life has no typed cause provenance"),
        }
        dead += 1;
    }
    assert!(dead > 0, "the provenance check exercised no dead lots");
}

#[test]
fn moved_life_cause_cites_the_ending_occupation() {
    let world = hornvale_worldgen::seed_42_world();
    let ctx = assemble(&world).unwrap();
    let life = (0..200_000)
        .filter_map(|index| draw(&ctx, LotIndex(index), &Pick::default()).ok())
        .find(|life| {
            let Some(moved) = life.moved_to else {
                return false;
            };
            life.ending_occupation != life.occupation
                && matches!(life.ending, Ending::CommunityFate(_))
                && ctx.occupations[life.occ].record.core.cause
                    != ctx.occupations[moved].record.core.cause
        })
        .expect("seed 42 yields a moved life with distinct birth and ending causes");
    let story = tell(&world, &ctx, &life);
    let cause = story.slot("cause").expect("cause slot exists");
    let ending = ctx
        .occupations
        .iter()
        .find(|prepared| prepared.record.id == life.ending_occupation)
        .expect("ending occupation is retained");
    assert!(matches!(
        &life.cause_provenance,
        Some(CauseProvenance::CommunityFate { occupation, cause })
            if *occupation == life.ending_occupation
                && Some(*cause) == ending.record.core.cause
    ));
    let committed_cause = ending
        .record
        .core
        .cause
        .expect("the ending occupation has a committed cause");
    let committed_object = world
        .ledger
        .facts_of(ending.record.id, hornvale_history::OCC_CAUSE)
        .next()
        .expect("ending occupation has an occ-cause fact")
        .object
        .clone();
    let committed_label = match committed_object {
        hornvale_kernel::Value::Text(label) => label,
        other => panic!("occ-cause has non-text object: {other:?}"),
    };
    let expected_label = match committed_cause {
        CauseOfEnd::Famine => "famine",
        CauseOfEnd::Burned => "burned",
        CauseOfEnd::Plague => "plague",
        CauseOfEnd::Fled => "fled",
        CauseOfEnd::Migrated => "migrated",
        CauseOfEnd::Breached => "breached",
    };
    assert_eq!(committed_label, expected_label);
    assert_eq!(life.cause, Some(DeathCause::Community(committed_cause)));
    assert_eq!(
        cause.value,
        SlotValue::Filled(life.cause.as_ref().unwrap().label())
    );
    assert!(!cause.sources.iter().any(|source| matches!(
        source,
        Source::Derived { function, .. } if *function == "lot::draw::hazard_cause"
    )));
    assert!(cause.sources.iter().any(|source| matches!(
        source,
        Source::Fact { entity, predicate, .. }
            if *entity == ending.record.id.get()
                && predicate == hornvale_history::OCC_CAUSE
    )));
    assert!(matches!(cause.value, SlotValue::Filled(_)));
}

#[test]
fn payload_adds_cause_projection_and_cause_odds() {
    let world = hornvale_worldgen::seed_42_world();
    let ctx = assemble(&world).unwrap();
    let life = (0..200)
        .find_map(|index| {
            let life = draw(&ctx, LotIndex(index), &Pick::default()).ok()?;
            (life.ending != Ending::Alive).then_some(life)
        })
        .expect("seed 42 yields a dead lot");
    let story = tell(&world, &ctx, &life);
    let life_doc: serde_json::Value =
        serde_json::from_str(&life_json(&ctx, &life, &story)).unwrap();
    assert!(life_doc["ending"]["cause"].is_string());
    assert_eq!(life_doc["projection"]["kind"], "composite");
    assert_eq!(life_doc["projection"]["consequences_write_back"], false);

    let odds_doc: serde_json::Value =
        serde_json::from_str(&odds_json(&odds_at(&ctx, life.occ, life.birth_year))).unwrap();
    let causes = odds_doc["causes"]
        .as_array()
        .expect("odds.causes is additive");
    assert!(!causes.is_empty());
    let sum: f64 = causes
        .iter()
        .map(|row| row["share"].as_f64().unwrap())
        .sum();
    assert!(
        (sum - 1.0).abs() < 1e-6,
        "emitted cause shares sum to {sum}"
    );
}

#[test]
fn composite_life_is_narrated_as_non_causal() {
    let world = hornvale_worldgen::seed_42_world();
    let ctx = assemble(&world).unwrap();
    let life = draw(&ctx, LotIndex(0), &Pick::default()).unwrap();
    let story = tell(&world, &ctx, &life);
    let prose = narrate(&ctx, &life, &story);
    assert!(prose.contains("a non-causal composite case"));
    assert!(prose.contains("cannot write consequences back"));
}

#[test]
fn composite_projection_refuses_persistent_write_back() {
    let cohort = SourceCohort {
        people: KindId("human"),
        site: Vertex(7),
        year: 1200.0,
    };
    let composite = Projection::composite(cohort, true);
    assert_eq!(composite.materiality, ProjectionMateriality::InWorld);
    assert!(!composite.consequences_write_back());
    let mut wrote = false;
    let refused = composite.write_persistent_consequence(|| wrote = true);
    assert!(refused.is_err());
    assert!(!wrote, "a composite projection executed a persistent write");

    let materialized = Projection::materialized_individual(cohort);
    materialized
        .write_persistent_consequence(|| wrote = true)
        .expect("a materialized individual may persist consequences");
    assert!(wrote);
}

#[test]
fn same_seed_still_builds_byte_identical_worlds_and_lot_payloads() {
    let seed = hornvale_kernel::Seed(42);
    let sky = hornvale_astronomy::SkyPins::default();
    let terrain = hornvale_terrain::TerrainPins::default();
    let settlements = hornvale_worldgen::SettlementPins::default();
    let components = hornvale_worldgen::WorldComponents::assemble().unwrap();
    let first_world = hornvale_worldgen::build_world(seed, &sky, &terrain, &settlements).unwrap();
    let second_world = hornvale_worldgen::build_world_to(
        seed,
        &sky,
        &terrain,
        &settlements,
        &components,
        hornvale_worldgen::BuildDepth::Full,
    )
    .unwrap();
    assert_eq!(
        serde_json::to_vec(&first_world).unwrap(),
        serde_json::to_vec(&second_world).unwrap()
    );
    let first_ctx = assemble(&first_world).unwrap();
    let second_ctx = assemble(&second_world).unwrap();
    let first_life = draw(&first_ctx, LotIndex(0), &Pick::default()).unwrap();
    let second_life = draw(&second_ctx, LotIndex(0), &Pick::default()).unwrap();
    let first_story = tell(&first_world, &first_ctx, &first_life);
    let second_story = tell(&second_world, &second_ctx, &second_life);
    assert_eq!(
        life_json(&first_ctx, &first_life, &first_story),
        life_json(&second_ctx, &second_life, &second_story)
    );
}
