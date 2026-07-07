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
  phenomena [day]  salient phenomena (default day 0)
  facts <id>       every fact about entity id
  quit             leave
";

/// Run the REPL over a world until `quit` or EOF.
pub fn run(world: &World, input: impl BufRead, mut output: impl Write) -> std::io::Result<()> {
    writeln!(
        output,
        "hornvale repl — world of seed {} ('help' for commands)",
        world.seed.0
    )?;
    for line in input.lines() {
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
                                climate.mean_temperature_at(cell),
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
            "beliefs" => {
                let beliefs = hornvale_religion::beliefs_of(world);
                if beliefs.is_empty() {
                    writeln!(output, "no beliefs are recorded")?;
                }
                for (i, belief) in beliefs.iter().enumerate() {
                    writeln!(output, "{}. [{}] {}", i + 1, belief.id.0, belief.tenet)?;
                }
            }
            "why" => match argument.and_then(|a| a.parse::<u64>().ok()) {
                Some(id) => match hornvale_historiography::recount(world, EntityId(id)) {
                    Some(text) => write!(output, "{text}")?,
                    None => writeln!(output, "nothing is recorded about entity {id}")?,
                },
                None => writeln!(
                    output,
                    "usage: why <entity-id> (ids from `beliefs`, `places`)"
                )?,
            },
            "phenomena" => {
                let day = argument.and_then(|a| a.parse().ok()).unwrap_or(0.0);
                match world_builder::observed_phenomena(world, day) {
                    Ok(phenomena) => {
                        for p in phenomena {
                            writeln!(output, "[{:.2}] {} — {}", p.salience, p.kind, p.description)?;
                        }
                    }
                    Err(e) => writeln!(output, "error: {e}")?,
                }
            }
            "facts" => {
                let id = argument.and_then(|a| a.parse::<u64>().ok());
                match id {
                    Some(id) => {
                        for f in world.ledger.facts_about(EntityId(id)) {
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
            other => writeln!(output, "unknown command '{other}' — try 'help'")?,
        }
    }
    Ok(())
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

    fn drive(commands: &str) -> String {
        let world = build_world(
            Seed(42),
            &SkyPins::default(),
            SkyChoice::Constant,
            &hornvale_terrain::TerrainPins::default(),
            &world_builder::SettlementPins::default(),
        )
        .unwrap();
        let mut out = Vec::new();
        run(&world, commands.as_bytes(), &mut out).unwrap();
        String::from_utf8(out).unwrap()
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
        // least a worker and a chief (`structure`'s invariant), but which
        // higher roles appear depends on its actual environment.
        assert!(out.contains("chief"));
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
        // The OR tolerates entity-numbering shifts (the world entity is
        // minted first as of 2b); either subject is a legitimate answer.
        assert!(out.contains("(settlement)") || out.contains("(terrain)"));
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
