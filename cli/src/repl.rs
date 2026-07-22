//! The REPL window: interrogate a world line by line. Generic over
//! input/output so tests drive it with buffers.

use hornvale_kernel::{EntityId, Value, World, WorldTime};
use hornvale_worldgen as world_builder;
use std::io::{BufRead, Write};

const HELP: &str = "\
commands:
  sky [day]        what the sky looks like (default day 0)
  climate          local climate
  map              ASCII elevation map of the globe
  biomes           biome map of the globe
  land <lat> <lon> the terrain at a coordinate (degrees)
  biome <lat> <lon> the biome at a coordinate (degrees)
  calendar         the world's cycles
  places           known places
  settlements      every settlement: name, population, biome
  settlement <lat> <lon> the settlement nearest a coordinate (degrees)
  village          the settlement
  castes           the settlement's castes
  beliefs          recorded beliefs
  why <id>         recount how an entity came to be (ids from beliefs/places)
  word <concept>   every species' word for a concept, or its reasoned gap
  phenomena [day] [--as <species>] — salient phenomena, optionally through a species' eyes
  facts <id>       every fact about entity id
  possess          walk the world as its flagship settler ('release' returns)
  quit             leave
";

/// Run the REPL over a world until `quit` or EOF.
pub fn run(world: &World, input: impl BufRead, mut output: impl Write) -> std::io::Result<()> {
    writeln!(
        output,
        "hornvale repl — world of seed {} ('help' for commands)",
        world.seed.0
    )?;
    let mut lines = input.lines();
    while let Some(line) = lines.next() {
        let line = line?;
        let mut parts = line.split_whitespace();
        let command = parts.next().unwrap_or("");
        let argument = parts.next();
        match command {
            "" => {}
            "quit" | "exit" => break,
            "help" => write!(output, "{HELP}")?,
            "sky" => {
                let day = argument.and_then(|a| a.parse().ok()).unwrap_or(0.0);
                match world_builder::sky_report(world, WorldTime { day }) {
                    Ok(report) => writeln!(output, "{}", report.description)?,
                    Err(e) => writeln!(output, "error: {e}")?,
                }
            }
            "climate" => {
                let report = world_builder::climate_report(world);
                writeln!(
                    output,
                    "{} ({:.0}°C)",
                    report.description, report.temperature_c
                )?;
            }
            "map" => match world_builder::terrain_of(world) {
                Ok(terrain) => write!(
                    output,
                    "{}",
                    hornvale_terrain::render::elevation_ascii(terrain.geosphere(), terrain.globe())
                )?,
                Err(e) => writeln!(output, "error: {e}")?,
            },
            "land" => {
                let coords = argument
                    .and_then(|lat| Some((lat, parts.next()?)))
                    .and_then(|(lat, lon)| {
                        Some((lat.parse::<f64>().ok()?, lon.parse::<f64>().ok()?))
                    });
                match coords {
                    None => writeln!(output, "usage: land <latitude> <longitude>")?,
                    Some((lat, lon)) => match world_builder::terrain_of(world) {
                        Ok(terrain) => {
                            let cell = terrain.nearest_cell(lat, lon);
                            let relative = terrain.elevation_at(cell) - terrain.sea_level();
                            let surface = if terrain.is_ocean(cell) {
                                format!("ocean, {:.0} m deep", -relative)
                            } else {
                                format!("land, {relative:.0} m above the sea")
                            };
                            writeln!(
                                output,
                                "cell {}: {surface}; plate {}; unrest {:.2}",
                                cell.0,
                                terrain.plate_of(cell),
                                terrain.unrest_at(cell)
                            )?;
                        }
                        Err(e) => writeln!(output, "error: {e}")?,
                    },
                }
            }
            "biomes" => match world_builder::climate_of(world) {
                Ok(climate) => write!(
                    output,
                    "{}",
                    hornvale_climate::render::biome_ascii(
                        climate.geosphere(),
                        &climate.biome_map()
                    )
                )?,
                Err(e) => writeln!(output, "error: {e}")?,
            },
            "biome" => {
                let coords = argument
                    .and_then(|lat| Some((lat, parts.next()?)))
                    .and_then(|(lat, lon)| {
                        Some((lat.parse::<f64>().ok()?, lon.parse::<f64>().ok()?))
                    });
                match coords {
                    None => writeln!(output, "usage: biome <latitude> <longitude>")?,
                    Some((lat, lon)) => match (
                        world_builder::terrain_of(world),
                        world_builder::climate_of(world),
                    ) {
                        (Ok(terrain), Ok(climate)) => {
                            let cell = terrain.nearest_cell(lat, lon);
                            writeln!(
                                output,
                                "cell {}: biome {} — {:.0}°C, moisture {:.2}",
                                cell.0,
                                climate.biome_at(cell).name(),
                                climate.mean_temperature_at(cell).get(),
                                climate.moisture_at(cell)
                            )?;
                        }
                        (Err(e), _) | (_, Err(e)) => writeln!(output, "error: {e}")?,
                    },
                }
            }
            "calendar" => match world_builder::calendar_lines(world) {
                Ok(lines) if lines.is_empty() => writeln!(
                    output,
                    "this world has no generated sky; time is measured in standard days"
                )?,
                Ok(lines) => {
                    for line in lines {
                        writeln!(output, "{line}")?;
                    }
                }
                Err(e) => writeln!(output, "error: {e}")?,
            },
            "places" => {
                for place in hornvale_terrain::places(world) {
                    writeln!(
                        output,
                        "{} — {} (entity {})",
                        place.name, place.biome, place.id.0
                    )?;
                }
            }
            "settlements" => {
                for place in hornvale_terrain::places(world) {
                    let population = match world
                        .ledger
                        .value_of(place.id, hornvale_settlement::POPULATION)
                    {
                        Some(Value::Number(n)) => *n as u32,
                        _ => 0,
                    };
                    writeln!(
                        output,
                        "{} — population {} — {}",
                        place.name, population, place.biome
                    )?;
                }
            }
            "settlement" => {
                let coords = argument
                    .and_then(|lat| Some((lat, parts.next()?)))
                    .and_then(|(lat, lon)| {
                        Some((lat.parse::<f64>().ok()?, lon.parse::<f64>().ok()?))
                    });
                match coords {
                    None => writeln!(output, "usage: settlement <latitude> <longitude>")?,
                    Some((lat, lon)) => match world_builder::terrain_of(world) {
                        Ok(terrain) => {
                            let cell = terrain.nearest_cell(lat, lon);
                            let found = hornvale_terrain::places(world).into_iter().find(|p| {
                                matches!(
                                    world.ledger.value_of(p.id, hornvale_settlement::CELL_ID),
                                    Some(Value::Number(n)) if *n as u32 == cell.0
                                )
                            });
                            match found {
                                Some(place) => writeln!(
                                    output,
                                    "{} — {} (entity {})",
                                    place.name, place.biome, place.id.0
                                )?,
                                None => writeln!(output, "no settlement on this cell")?,
                            }
                        }
                        Err(e) => writeln!(output, "error: {e}")?,
                    },
                }
            }
            "village" => match hornvale_settlement::village_info(world) {
                Some(v) => writeln!(
                    output,
                    "{} — population {} (entity {})",
                    v.name, v.population, v.id.0
                )?,
                None => writeln!(output, "no settlement is known")?,
            },
            "castes" => match hornvale_settlement::village_info(world) {
                Some(v) => {
                    let castes = hornvale_culture::castes_of(world, v.id);
                    writeln!(output, "{}", castes.join(", "))?;
                }
                None => writeln!(output, "no settlement is known")?,
            },
            "beliefs" => match world_builder::rendered_beliefs(world) {
                Ok(rendered) => {
                    if rendered.is_empty() {
                        writeln!(output, "no beliefs are recorded")?;
                    }
                    // Each tenet is rendered through the content→render seam
                    // (spec §6): `hornvale_language::render_line` voiced by
                    // the holding species' psychology-derived `VoiceParams`.
                    for (i, (belief, tenet)) in rendered.iter().enumerate() {
                        writeln!(output, "{}. [{}] {}", i + 1, belief.id.0, tenet)?;
                    }
                }
                Err(e) => writeln!(output, "error: {e}")?,
            },
            "why" => match argument.and_then(|a| a.parse::<u64>().ok()) {
                Some(id) => match EntityId::new(id).and_then(|target| {
                    hornvale_historiography::recount(world, target).map(|text| (target, text))
                }) {
                    Some((target, text)) => {
                        write!(output, "{text}")?;
                        // A belief recounts onward to the eyes that ranked
                        // its phenomenon: held-by → settlement → peopled-by
                        // → the species entity's authored vector.
                        if let Some(Value::Entity(community)) =
                            world.ledger.value_of(target, hornvale_religion::HELD_BY)
                            && let Some(species) = hornvale_species::species_of(world, *community)
                            && let Some(entity) = hornvale_species::species_entity(world, &species)
                            && let Some(text) = hornvale_historiography::recount(world, entity)
                        {
                            writeln!(output, "Seen through {species} eyes:")?;
                            write!(output, "{text}")?;
                        }
                        // A generated proper name carries a `name-gloss`
                        // fact (Task 9 of The Words) when its site had a
                        // true story to tell; recount it too.
                        if let Some(gloss) =
                            world.ledger.text_of(target, hornvale_kernel::NAME_GLOSS)
                        {
                            writeln!(
                                output,
                                "named for: {gloss} ({})",
                                site_facts_of(world, gloss)
                            )?;
                        }
                    }
                    None => writeln!(output, "nothing is recorded about entity {id}")?,
                },
                None => writeln!(
                    output,
                    "usage: why <entity-id> (ids from `beliefs`, `places`)"
                )?,
            },
            "phenomena" => {
                // `argument` already popped one token from `parts`; the rest
                // of the line (e.g. the species name after `--as`) is still
                // sitting in `parts`, so both must be chained together.
                let tokens: Vec<&str> = argument.into_iter().chain(parts).collect();
                let species = tokens
                    .iter()
                    .position(|t| *t == "--as")
                    .and_then(|i| tokens.get(i + 1).copied());
                let result = match species {
                    Some(s) => world_builder::observed_phenomena_as(world, s),
                    None => {
                        let day = tokens.first().and_then(|a| a.parse().ok()).unwrap_or(0.0);
                        world_builder::observed_phenomena(world, day)
                    }
                };
                match result {
                    Ok(phenomena) => {
                        for p in phenomena {
                            writeln!(output, "[{:.2}] {} — {}", p.salience, p.kind, p.description)?;
                        }
                    }
                    Err(e) => writeln!(output, "error: {e}")?,
                }
            }
            "word" => match argument {
                Some(concept) => {
                    if world.registry.concept(concept).is_none() {
                        writeln!(output, "unknown concept '{concept}'")?;
                    } else {
                        // peopled-only: fauna never speak, so `lexicon_of`
                        // is undefined for them. `psyche_registry()` is keyed
                        // by exactly the four speaking peoples.
                        for species in hornvale_species::psyche_registry().ids().map(|k| k.0) {
                            match world_builder::lexicon_of(world, species) {
                                Ok(lexicon) => match lexicon.entry(concept) {
                                    Some(entry) => writeln!(
                                        output,
                                        "{species}: {}",
                                        crate::dictionary::word_line(entry)
                                    )?,
                                    None => {
                                        writeln!(output, "{species}: no entry for '{concept}'")?
                                    }
                                },
                                Err(e) => writeln!(output, "{species}: error: {e}")?,
                            }
                        }
                    }
                }
                None => writeln!(output, "usage: word <concept>")?,
            },
            "facts" => {
                let id = argument
                    .and_then(|a| a.parse::<u64>().ok())
                    .and_then(EntityId::new);
                match id {
                    Some(id) => {
                        for f in world.ledger.facts_about(id) {
                            writeln!(
                                output,
                                "{} = {} ({})",
                                f.predicate,
                                render_value(&f.object),
                                f.provenance
                            )?;
                        }
                    }
                    None => writeln!(output, "usage: facts <entity-id>")?,
                }
            }
            "possess" => {
                let opts = hornvale_vessel::PossessOpts {
                    day: WorldTime { day: 0.0 },
                    echo: false,
                    wild_agents: true,
                };
                match hornvale_vessel::Session::start(world, &opts) {
                    Err(e) => writeln!(output, "error: {e}")?,
                    Ok((mut session, opening)) => {
                        writeln!(output, "{opening}")?;
                        for inner in lines.by_ref() {
                            let inner = inner?;
                            match session.handle(&inner) {
                                hornvale_vessel::Turn::Out(s) => {
                                    if !s.is_empty() {
                                        writeln!(output, "{s}")?;
                                    }
                                }
                                hornvale_vessel::Turn::Released(s) => {
                                    writeln!(output, "{s}")?;
                                    break;
                                }
                            }
                        }
                    }
                }
            }
            other => writeln!(output, "unknown command '{other}' — try 'help'")?,
        }
    }
    Ok(())
}

/// Decompose a `name-gloss` fact's value back into the registered concept
/// id(s) that produced it — [`hornvale_language::Namer::glossed_name`]
/// always joins exactly the 1-2 chosen site concepts with `"-"` (see
/// `naming.rs`). Concept ids may themselves contain a hyphen, so the gloss
/// text alone can be ambiguous: today's registry really does hold `sea`,
/// `ice`, AND `sea-ice`, making the gloss "sea-ice" readable as either the
/// one biome concept or a sea+ice join. The rule: collect every
/// interpretation ([`interpretations_of`]) and render the decomposition
/// only when exactly one exists; zero (a save whose registry no longer
/// holds a concept the gloss once named) or several (ambiguity) echo the
/// raw gloss unchanged — a fallback, never a guess.
fn site_facts_of(world: &World, gloss: &str) -> String {
    let names: Vec<&str> = world.registry.concepts().map(|c| c.name.as_str()).collect();
    let mut found = interpretations_of(&names, gloss);
    match found.len() {
        1 => found.remove(0),
        _ => gloss.to_string(),
    }
}

/// Every reading of `gloss` as 1-2 registered concept ids: the whole gloss
/// as one concept (rendered as itself), plus every ordered two-concept pair
/// whose `"-"`-join reproduces it (rendered `"a, b"`), in `names` order.
/// [`site_facts_of`] renders a decomposition only when this returns exactly
/// one entry.
fn interpretations_of(names: &[&str], gloss: &str) -> Vec<String> {
    let mut found = Vec::new();
    if names.contains(&gloss) {
        found.push(gloss.to_string());
    }
    for &a in names {
        for &b in names {
            if format!("{a}-{b}") == gloss {
                found.push(format!("{a}, {b}"));
            }
        }
    }
    found
}

fn render_value(value: &Value) -> String {
    match value {
        Value::Text(t) => t.clone(),
        Value::Number(n) => n.to_string(),
        Value::Flag(b) => b.to_string(),
        Value::Entity(e) => format!("entity {}", e.0),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_astronomy::SkyPins;
    use hornvale_kernel::Seed;
    use world_builder::{SkyChoice, build_world};

    fn constant_world() -> World {
        build_world(
            Seed(42),
            &SkyPins::default(),
            SkyChoice::Constant,
            &hornvale_terrain::TerrainPins::default(),
            &world_builder::SettlementPins::default(),
        )
        .unwrap()
    }

    fn drive(commands: &str) -> String {
        let world = constant_world();
        let mut out = Vec::new();
        run(&world, commands.as_bytes(), &mut out).unwrap();
        String::from_utf8(out).unwrap()
    }

    #[test]
    fn possess_hands_off_and_release_returns_to_the_scholar_loop() {
        let world = constant_world();
        let input = b"possess\nlook\nrelease\nvillage\nquit\n" as &[u8];
        let mut output = Vec::new();
        run(&world, input, &mut output).unwrap();
        let text = String::from_utf8(output).unwrap();
        assert!(text.contains("You stand in"), "possession opened");
        assert!(text.contains("You let go."), "release ended the possession");
        let released_at = text.find("You let go.").unwrap();
        let village_out = &text[released_at..];
        assert!(
            village_out.lines().count() > 1,
            "the scholar loop answered 'village' after release"
        );
    }

    #[test]
    fn site_facts_of_an_ambiguous_gloss_falls_back_to_the_raw_text() {
        // The triple that makes ambiguity real in today's registry: `sea`
        // (terrain), `ice` (climate), AND `sea-ice` (the biome) are all
        // registered, so the gloss "sea-ice" is readable as either the one
        // biome concept or a sea+ice join. Ambiguity must fall back to the
        // raw gloss — never a guess between the two readings.
        let world = constant_world();
        let names: Vec<&str> = world.registry.concepts().map(|c| c.name.as_str()).collect();
        for concept in ["sea", "ice", "sea-ice"] {
            assert!(
                names.contains(&concept),
                "precondition: {concept} must be registered for this test to bite"
            );
        }
        assert_eq!(
            interpretations_of(&names, "sea-ice").len(),
            2,
            "sea-ice must be seen as both a whole concept and a sea+ice join"
        );
        assert_eq!(
            site_facts_of(&world, "sea-ice"),
            "sea-ice",
            "an ambiguous gloss must echo itself, never guess a decomposition"
        );
    }

    #[test]
    fn site_facts_of_a_unique_join_gloss_names_its_two_concepts() {
        let world = constant_world();
        let names: Vec<&str> = world.registry.concepts().map(|c| c.name.as_str()).collect();
        assert!(
            !names.contains(&"ice-home"),
            "precondition: ice-home must not itself be a registered concept"
        );
        assert_eq!(site_facts_of(&world, "ice-home"), "ice, home");
    }

    #[test]
    fn site_facts_of_a_whole_concept_gloss_echoes_itself_across_the_entire_registry() {
        // The registry-wide decomposition regression the sea/ice/sea-ice
        // triple motivates: a gloss that IS a whole registered concept must
        // never render as a pair — either it is the unique interpretation
        // (rendered as itself) or it collides with a join (ambiguous →
        // fallback, also itself). Sweeping every registered concept keeps
        // this true as the inventory grows.
        let world = constant_world();
        for c in world.registry.concepts() {
            assert_eq!(
                site_facts_of(&world, &c.name),
                c.name,
                "whole-concept gloss {} must echo itself, not decompose",
                c.name
            );
        }
    }

    #[test]
    fn sky_reports_the_constant_sun() {
        assert!(drive("sky\nquit\n").contains("zenith"));
    }

    #[test]
    fn calendar_on_constant_world_says_no_generated_sky() {
        assert!(drive("calendar\nquit\n").contains("no generated sky"));
    }

    #[test]
    fn calendar_on_generated_world_reports_the_year() {
        let world = build_world(
            Seed(42),
            &SkyPins::default(),
            SkyChoice::Generated,
            &hornvale_terrain::TerrainPins::default(),
            &world_builder::SettlementPins::default(),
        )
        .unwrap();
        let mut out = Vec::new();
        run(&world, "calendar\nquit\n".as_bytes(), &mut out).unwrap();
        let out = String::from_utf8(out).unwrap();
        assert!(out.contains("year is"));
    }

    #[test]
    fn village_and_castes_and_beliefs_report() {
        let out = drive("village\ncastes\nbeliefs\nquit\n");
        assert!(out.contains("population"));
        // Castes are emergent now (Campaign 4b): every settlement grows at
        // least a worker and a top rung (`structure`'s invariant), but which
        // higher roles appear depends on its actual environment. Re-pinned
        // under The Living Community epoch (history is the sole settlement
        // placer, this merge): the deep-history bake re-placed every world, so
        // the constant-sky flagship (entity 2, village Shngooshshngoash...) is
        // now peopled by bugbear, and the roles reported here are bugbear's own
        // words — "forager, omen-reader, headman", the top rung "headman".
        assert!(out.contains("headman"));
        assert!(out.contains("1."));
    }

    #[test]
    fn why_explains_belief_one() {
        let world = build_world(
            Seed(42),
            &SkyPins::default(),
            SkyChoice::Constant,
            &hornvale_terrain::TerrainPins::default(),
            &world_builder::SettlementPins::default(),
        )
        .unwrap();
        let beliefs = hornvale_religion::beliefs_of(&world);
        assert!(!beliefs.is_empty(), "test world has beliefs");
        let belief_id = beliefs[0].id.0;
        let mut out = Vec::new();
        run(
            &world,
            format!("why {belief_id}\nquit\n").as_bytes(),
            &mut out,
        )
        .unwrap();
        let out = String::from_utf8(out).unwrap();
        assert!(out.contains("celestial-body"));
    }

    #[test]
    fn facts_lists_an_entity() {
        let out = drive("facts 2\nquit\n");
        // Each fact line is tagged with the domain that asserted it. Under The
        // Living Community epoch (this merge), entity 2 is the flagship
        // settlement, whose is-settlement/population/cell-id facts are now
        // committed by the deep-history bake (tag "(history/bake)") rather than
        // the settlement domain. The OR tolerates entity-numbering shifts.
        assert!(out.contains("(history/bake)") || out.contains("(terrain)"));
    }

    #[test]
    fn map_and_land_answer_terrain_queries() {
        let out = drive("map\nland 45 -30\nquit\n");
        assert!(out.contains('~'), "no ascii ocean");
        assert!(out.contains("plate"), "no per-cell land report");
    }

    #[test]
    fn settlements_and_settlement_answer_queries() {
        let out = drive("settlements\nquit\n");
        assert!(out.contains("population"), "no settlements listing");

        let world = build_world(
            Seed(42),
            &SkyPins::default(),
            SkyChoice::Constant,
            &hornvale_terrain::TerrainPins::default(),
            &world_builder::SettlementPins::default(),
        )
        .unwrap();
        let village = hornvale_settlement::village_info(&world).expect("a village exists");
        let lat = match world
            .ledger
            .value_of(village.id, hornvale_settlement::LATITUDE)
        {
            Some(Value::Number(n)) => *n,
            _ => panic!("village has no latitude"),
        };
        let lon = match world
            .ledger
            .value_of(village.id, hornvale_settlement::LONGITUDE)
        {
            Some(Value::Number(n)) => *n,
            _ => panic!("village has no longitude"),
        };
        let mut out = Vec::new();
        run(
            &world,
            format!("settlement {lat} {lon}\nquit\n").as_bytes(),
            &mut out,
        )
        .unwrap();
        let out = String::from_utf8(out).unwrap();
        assert!(
            out.contains(&village.name),
            "expected the settlement nearest ({lat}, {lon}) to be reported; got: {out}"
        );
    }

    #[test]
    fn settlement_without_coordinates_reports_usage() {
        assert!(drive("settlement\nquit\n").contains("usage: settlement"));
    }

    #[test]
    fn unknown_commands_are_reported_not_fatal() {
        let out = drive("dance\nsky\nquit\n");
        assert!(out.contains("unknown command"));
        assert!(out.contains("zenith"));
    }

    #[test]
    fn eof_ends_the_loop_without_quit() {
        assert!(drive("sky\n").contains("zenith"));
    }

    #[test]
    fn biomes_and_biome_answer_queries() {
        let world = build_world(
            Seed(42),
            &SkyPins::default(),
            SkyChoice::Generated,
            &hornvale_terrain::TerrainPins::default(),
            &world_builder::SettlementPins::default(),
        )
        .unwrap();
        let mut out = Vec::new();
        run(&world, "biomes\nbiome 5 -40\nquit\n".as_bytes(), &mut out).unwrap();
        let out = String::from_utf8(out).unwrap();
        assert!(out.lines().count() > 24, "no ascii biome map");
        assert!(out.contains("biome"), "no per-cell biome report");
    }

    fn drive_generated(commands: &str) -> String {
        let world = build_world(
            Seed(42),
            &SkyPins::default(),
            SkyChoice::Generated,
            &hornvale_terrain::TerrainPins::default(),
            &world_builder::SettlementPins::default(),
        )
        .unwrap();
        let mut out = Vec::new();
        run(&world, commands.as_bytes(), &mut out).unwrap();
        String::from_utf8(out).unwrap()
    }

    #[test]
    fn phenomena_as_kobold_ranks_the_night_sky_first() {
        let out = drive_generated("phenomena --as kobold\n");
        // line 0 is the REPL's banner ("hornvale repl — world of seed …"),
        // printed unconditionally by `run`; the first *reported* phenomenon
        // is line 1.
        let first = out.lines().nth(1).unwrap();
        assert!(
            first.contains("moon") || first.contains("star"),
            "kobold-lensed ranking must be night-headed, got: {first}"
        );
    }

    #[test]
    fn phenomena_as_unknown_species_fails_loudly() {
        let out = drive_generated("phenomena --as elf\n");
        assert!(out.contains("unknown species 'elf'"));
        assert!(out.contains("goblin"), "the error lists known species");
    }

    #[test]
    fn word_reports_every_species_entry_for_a_steeped_concept() {
        // The universal stratum (Task 5) is Steeped for every species
        // unconditionally, so "water" always resolves to a root, never a
        // gap, on both shipped species.
        let out = drive_generated("word water\n");
        assert!(out.contains("goblin:"), "missing goblin's entry: {out}");
        assert!(out.contains("kobold:"), "missing kobold's entry: {out}");
        assert!(out.contains(" → "), "a root's line needs a derivation");
    }

    #[test]
    fn word_with_no_argument_reports_usage() {
        assert!(drive_generated("word\n").contains("usage: word"));
    }

    #[test]
    fn word_with_an_unregistered_concept_is_reported_not_fatal() {
        let out = drive_generated("word not-a-real-concept\ncalendar\n");
        assert!(out.contains("unknown concept 'not-a-real-concept'"));
        assert!(
            out.contains("year is"),
            "the REPL must keep running after: {out}"
        );
    }

    #[test]
    fn why_appends_named_for_when_a_name_gloss_fact_exists() {
        let world = build_world(
            Seed(42),
            &SkyPins::default(),
            SkyChoice::Generated,
            &hornvale_terrain::TerrainPins::default(),
            &world_builder::SettlementPins::default(),
        )
        .unwrap();
        let glossed_id = world
            .ledger
            .find(hornvale_settlement::IS_SETTLEMENT)
            .find(|f| {
                world
                    .ledger
                    .text_of(f.subject, hornvale_kernel::NAME_GLOSS)
                    .is_some()
            })
            .map(|f| f.subject.0)
            .expect("seed 42 glosses at least one settlement");
        let gloss = world
            .ledger
            .text_of(EntityId(glossed_id), hornvale_kernel::NAME_GLOSS)
            .unwrap()
            .to_string();
        let mut out = Vec::new();
        run(
            &world,
            format!("why {glossed_id}\nquit\n").as_bytes(),
            &mut out,
        )
        .unwrap();
        let out = String::from_utf8(out).unwrap();
        assert!(
            out.contains(&format!("named for: {gloss} (")),
            "missing the name-gloss recount line: {out}"
        );
    }

    #[test]
    fn why_a_belief_recounts_through_the_species_eyes() {
        let out = drive_generated("beliefs\n");
        // line 0 is the REPL's banner; the first listed belief is line 1.
        let first_id: u64 = out
            .lines()
            .nth(1)
            .and_then(|l| l.split(['[', ']']).nth(1))
            .and_then(|s| s.parse().ok())
            .expect("beliefs lists at least one id");
        let recounted = drive_generated(&format!("why {first_id}\n"));
        // The first pantheon in `beliefs`' listing is the one worldgen commits
        // first; the recount hops from that belief's holding community to the
        // species that peoples it. Re-pinned under The Living Community epoch
        // (history is the sole settlement placer, this merge): the deep-history
        // bake re-placed every world and changed which people commits the
        // generated seed-42 world's first pantheon — it is bugbear now, so the
        // recount hops through bugbear's eyes.
        assert!(
            recounted.contains("Seen through bugbear eyes:"),
            "the species hop is missing: {recounted}"
        );
        assert!(recounted.contains("night-sky acuity"));
    }

    #[test]
    fn why_recounts_any_entity_by_id() {
        let world = build_world(
            Seed(42),
            &SkyPins::default(),
            SkyChoice::Generated,
            &hornvale_terrain::TerrainPins::default(),
            &world_builder::SettlementPins::default(),
        )
        .unwrap();
        // The flagship belief entities are listed with ids by `beliefs`.
        let listing = {
            let mut o = Vec::new();
            run(&world, "beliefs\nquit\n".as_bytes(), &mut o).unwrap();
            String::from_utf8(o).unwrap()
        };
        assert!(
            listing.contains('['),
            "beliefs lists entity ids in brackets"
        );
        // Recount the flagship settlement (entity from village_info).
        let id = hornvale_settlement::village_info(&world).unwrap().id.0;
        let mut out = Vec::new();
        run(&world, format!("why {id}\nquit\n").as_bytes(), &mut out).unwrap();
        let out = String::from_utf8(out).unwrap();
        assert!(out.contains("asserted by"), "recount names provenance");
        // An unknown entity is reported, not fatal.
        let mut out2 = Vec::new();
        run(&world, "why 99999\nquit\n".as_bytes(), &mut out2).unwrap();
        assert!(
            String::from_utf8(out2)
                .unwrap()
                .contains("nothing is recorded")
        );
    }
}
